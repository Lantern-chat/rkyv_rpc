#![cfg_attr(not(feature = "codec"), no_std)]
#![doc = include_str!("../README.md")]

#[doc(hidden)]
pub extern crate alloc;
#[doc(hidden)]
pub extern crate paste;
#[doc(hidden)]
pub extern crate rkyv;

#[cfg(feature = "codec")]
pub mod codec;

#[cfg(feature = "bitflags")]
mod bitflags;

#[cfg(feature = "bitflags")]
#[doc(hidden)]
pub extern crate bitflags as og_bitflags;

#[doc(hidden)]
pub mod private {
    pub const fn mirror_tag_u16(t: u16) -> u32 {
        let le = t.to_le_bytes();
        u32::from_le_bytes([le[0], le[1], le[1], le[0]])
    }

    pub const fn mirror_tag_u32(t: u32) -> u64 {
        let le = t.to_le_bytes();
        u64::from_le_bytes([le[0], le[1], le[2], le[3], le[3], le[2], le[1], le[0]])
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! __rkyv_rpc_helper {
    (@REPR u8 $(#[$meta:meta])* $vis:vis enum $name:ident { $($tt:tt)* } ) => {
        $(#[$meta])* #[repr(u8, align(1))] $vis enum $name { $($tt)* }
    };
    (@REPR u16 $(#[$meta:meta])* $vis:vis enum $name:ident { $($tt:tt)* } ) => {
        $(#[$meta])* #[repr(u32, align(4))] $vis enum $name { $($tt)* }
    };
    (@REPR u32 $(#[$meta:meta])* $vis:vis enum $name:ident { $($tt:tt)* } ) => {
        $(#[$meta])* #[repr(u64, align(8))] $vis enum $name { $($tt)* }
    };
    (@MIRROR u8 $tag:literal) => { $tag };
    (@MIRROR u16 $tag:literal) => { $crate::private::mirror_tag_u16($tag) };
    (@MIRROR u32 $tag:literal) => { $crate::private::mirror_tag_u32($tag) };
    (@CAST u8) => { u8 };
    (@CAST u16) => { u32 };
    (@CAST u32) => { u64 };
}

// TODO: Palindrome

/// Implements [`rkyv::Archive`], [`rkyv::Serialize`], and [`rkyv::Deserialize`] for a 1-tuple enum
/// in such a way as to be endianness-agnostic.
///
/// If the repr is u8, the enum will have a native u8 repr. However,
/// if the repr is u16 or u32, the enum will be represented as a u32 or u64, respectively,
/// doubling the size of the discriminator.
///
/// This is because to make the discriminator endianness-agnostic, it must be represented
/// as a value that is symmetric in its byte representation. The bytes are mirrored.
///
/// Furthermore, all enum variants will be automatically wrapped in
/// [`Box`](alloc::boxed::Box) to the enum layout is consistent across all variants.
///
/// # Example
/// ```rust
/// rkyv_rpc::tuple_enum! {
///     pub enum Example: u16 {
///         1000 = A(u8),
///         1001 = B(u16),
///         1002 = C(String),
///     }
/// }
/// ```
#[macro_export]
macro_rules! tuple_enum {
    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident: $repr:ty {
            $($(#[$variant_meta:meta])* $code:literal = $variant:ident($ty:ty),)*
        }
    ) => {$crate::paste::paste! {
        $vis use [<$name:snake _impl>]::{$name, [<$name Resolver>], [<Archived $name>]};

        mod [<$name:snake _impl>] {
            #[allow(unused_imports)]
            use super::*; // import any types needed

            use $crate::alloc::boxed::Box;

            $(#[$meta])*
            #[repr($repr)]
            $vis enum $name {
                $($(#[$variant_meta])* $variant(Box<$ty>) = $code,)*
            }

            use $crate::rkyv::{Archive, Archived, Serialize, Deserialize, Place, Portable, Resolver};
            use $crate::rkyv::bytecheck::{CheckBytes, UnnamedEnumVariantCheckContext, InvalidEnumDiscriminantError};
            use $crate::rkyv::rancor::{Fallible, Trace, Source};

            $(
                impl From<$ty> for $name {
                    fn from(value: $ty) -> Self {
                        Self::$variant(Box::new(value))
                    }
                }

                impl From<Box<$ty>> for $name {
                    fn from(value: Box<$ty>) -> Self {
                        Self::$variant(value)
                    }
                }

                #[repr(C)]
                struct [<Archived $variant Variant>] {
                    tag: ArchivedTag,
                    data: Archived<Box<$ty>>,
                    mkr: core::marker::PhantomData<$name>,
                }
            )*

            $crate::__rkyv_rpc_helper! {
                @REPR $repr
                #[derive(Debug, Clone, Copy, PartialEq, Eq)]

                pub enum ArchivedTag {
                    $($variant = $crate::__rkyv_rpc_helper!(@MIRROR $repr $code),)*
                }
            }

            #[allow(non_upper_case_globals)]
            impl ArchivedTag {
                $(const [<$variant Discriminant>]: $crate::__rkyv_rpc_helper!(@CAST $repr) = ArchivedTag::$variant as _;)*
            }

            $crate::__rkyv_rpc_helper! {
                @REPR $repr
                #[doc = "Archived version of `" $name "`"]
                pub enum [<Archived $name>] {
                    $($variant(Archived<Box<$ty>>) = ArchivedTag::$variant as _,)*
                }
            }

            // SAFETY: This is safe because the tags are mirrored and endianness is preserved
            unsafe impl Portable for [<Archived $name>] {}

            #[repr($repr)]
            pub enum [<$name Resolver>] {
                $($variant(Resolver<Box<$ty>>) = $code,)*
            }

            impl Archive for $name {
                type Archived = [<Archived $name>];
                type Resolver = [<$name Resolver>];

                fn resolve(&self, resolver: Self::Resolver, out: Place<Self::Archived>) {
                    match resolver {$(
                        [<$name Resolver>]::$variant(resolver_0) => match self {
                            $name::$variant(self_0) => {
                                <Box<$ty> as Archive>::resolve(self_0, resolver_0, unsafe {
                                    let out = out.cast_unchecked::<[<Archived $variant Variant>]>();
                                    core::ptr::addr_of_mut!((*out.ptr()).tag).write(ArchivedTag::$variant);
                                    Place::from_field_unchecked(out, core::ptr::addr_of_mut!((*out.ptr()).data))
                                });
                            },

                            #[allow(unreachable_patterns)]
                            _ => unsafe { core::hint::unreachable_unchecked() },
                        },
                    )*}
                }
            }

            impl<S: Fallible + ?Sized> Serialize<S> for $name
                where $(Box<$ty>: Serialize<S>,)*
            {
                fn serialize(&self, serializer: &mut S) -> Result<[<$name Resolver>], S::Error> {
                    Ok(match self {
                        $($name::$variant(data) => [<$name Resolver>]::$variant(Serialize::serialize(data, serializer)?),)*
                    })
                }
            }

            impl<D: Fallible + ?Sized> Deserialize<$name, D> for [<Archived $name>]
                where $(Archived<Box<$ty>>: Deserialize<Box<$ty>, D>,)*
            {
                fn deserialize(&self, deserializer: &mut D) -> Result<$name, D::Error> {
                    Ok(match self {$(
                        [<Archived $name>]::$variant(data) => $name::$variant(Deserialize::deserialize(data, deserializer)?),
                    )*})
                }
            }

            unsafe impl<C> CheckBytes<C> for [<Archived $name>]
            where
                C: Fallible + ?Sized,
                <C as Fallible>::Error: Source,
                $(Archived<Box<$ty>>: CheckBytes<C>,)*
            {
                unsafe fn check_bytes(value: *const Self, context: &mut C) -> Result<(), <C as Fallible>::Error> {
                    Ok(match *value.cast::<$crate::__rkyv_rpc_helper!(@CAST $repr)>() {
                        $(ArchivedTag::[<$variant Discriminant>] => {
                            let value = value.cast::<[<Archived $variant Variant>]>();
                            CheckBytes::check_bytes(core::ptr::addr_of!((*value).data), context).map_err(|e| {
                                <<C as Fallible>::Error as Trace>::trace(e, UnnamedEnumVariantCheckContext {
                                    enum_name: stringify!([<Archived $name>]),
                                    variant_name: stringify!($variant),
                                    field_index: 1,
                                })
                            })?;
                        },)*

                        invalid_discriminant => return Err(<<C as Fallible>::Error as Source>::new(InvalidEnumDiscriminantError {
                            enum_name: stringify!([<Archived $name>]),
                            invalid_discriminant,
                        })),
                    })
                }
            }
        }
    }};
}

/// Implements [`rkyv::Archive`], [`rkyv::Serialize`], and [`rkyv::Deserialize`] for an unit enum
/// in such a way as to be endianness-agnostic.
///
/// This is similar to [`tuple_enum!`], but for unit enums. `rkyv` naturally only
/// supports `u8` discriminants with its derive macros, so this macro is necessary to
/// support larger endianness-agnostic discriminants.
///
/// # Example
///
/// This fails because rkyv forces a u8 repr for the archived discriminant:
/// ```rust,compile_fail
/// #[derive(rkyv::Archive)]
/// pub enum Example {
///    A = 1000,
///    B = 1001,
///    C = 1002,
/// }
/// ```
///
/// This works because the discriminant is stored as a u32 and mirrored in the archive:
/// ```rust
///rkyv_rpc::unit_enum! {
///     pub enum Example: u16 {
///         1000 = A,
///         1001 = B,
///         1002 = C,
///     }
/// }
/// ```
///
/// This macro also accepts a `pub enum Test: u16 = Variant` form to specify a default variant to use when
/// an unknown discriminant is encountered during deserialization. This can be useful for backwards compatibility.
/// However, this means any unknown discriminants pass validation checks.
#[macro_export]
macro_rules! unit_enum {
    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident: $repr:ty $(= $unknown:ident)? {
            $($(#[$variant_meta:meta])* $code:literal = $variant:ident,)*
        }
    ) => {$crate::paste::paste! {
        use [<$name:snake _impl>]::{$name, [<Archived $name>]};

        mod [<$name:snake _impl>] {
            $(#[$meta])*
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            #[repr($repr)]
            $vis enum $name {
                $($(#[$variant_meta])* $variant = $code,)*
            }

            use $crate::rkyv::{Archive, Serialize, Deserialize, Portable};
            use $crate::rkyv::place::{Place, Initialized};
            use $crate::rkyv::traits::CopyOptimization;
            use $crate::rkyv::bytecheck::{CheckBytes, InvalidEnumDiscriminantError};
            use $crate::rkyv::rancor::{Fallible, Source};

            $crate::__rkyv_rpc_helper! {
                @REPR $repr
                #[derive(Debug, Clone, Copy, PartialEq, Eq)]
                pub enum [<Archived $name>] {
                    $($variant = $crate::__rkyv_rpc_helper!(@MIRROR $repr $code),)*
                }
            }

            #[allow(non_upper_case_globals)]
            impl [<Archived $name>] {
                $(const [<$variant Discriminant>]: $crate::__rkyv_rpc_helper!(@CAST $repr) = [<Archived $name>]::$variant as _;)*
            }

            // SAFETY: This is safe because the tags are mirrored and endianness is preserved
            unsafe impl Portable for [<Archived $name>] {}
            unsafe impl Initialized for [<Archived $name>] {}

            impl Archive for $name {
                type Archived = [<Archived $name>];
                type Resolver = ();

                const COPY_OPTIMIZATION: CopyOptimization<Self> = unsafe {
                    CopyOptimization::enable()
                };

                #[inline]
                fn resolve(&self, _: Self::Resolver, out: Place<Self::Archived>) {
                    match *self {
                        $($name::$variant => out.write([<Archived $name>]::$variant),)*
                    }
                }
            }

            impl<S: Fallible + ?Sized> Serialize<S> for $name {
                #[inline]
                fn serialize(&self, _: &mut S) -> Result<(), S::Error> {
                    Ok(())
                }
            }

            impl<D: Fallible + ?Sized> Deserialize<$name, D> for [<Archived $name>] {
                #[inline]
                fn deserialize(&self, _: &mut D) -> Result<$name, D::Error> {
                    Ok(match *self {
                        $(<[<Archived $name>]>::$variant => $name::$variant,)*
                        $(_ => $name::$unknown,)?
                    })
                }
            }

            unsafe impl<C> CheckBytes<C> for [<Archived $name>]
            where
                C: Fallible + ?Sized,
                <C as Fallible>::Error: Source,
            {
                unsafe fn check_bytes(value: *const Self, _: &mut C) -> Result<(), <C as Fallible>::Error> {
                    Ok(match *value.cast::<$crate::__rkyv_rpc_helper!(@CAST $repr)>() {
                        $([<Archived $name>]::[<$variant Discriminant>] => (),)*
                        $(_ => { _ = $name::$unknown; })?

                        invalid_discriminant => return Err(<<C as Fallible>::Error as Source>::new(InvalidEnumDiscriminantError {
                            enum_name: stringify!([<Archived $name>]),
                            invalid_discriminant,
                        })),
                    })
                }
            }
        }
    }};
}
