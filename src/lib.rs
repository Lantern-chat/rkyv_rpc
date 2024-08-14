#![cfg_attr(not(feature = "codec"), no_std)]
#![doc = include_str!("../README.md")]

#[doc(hidden)]
pub extern crate alloc;
#[doc(hidden)]
pub extern crate rkyv;

#[cfg(feature = "codec")]
pub mod codec;

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

/// Implements [`rkyv::Archive`], [`rkyv::Serialize`], and [`rkyv::Deserialize`] for an enum
/// for such a way as to be endianness-agnostic.
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
/// rkyv_rpc::rpc_enum! {
///     pub enum Example: u16 {
///         0 = A(u8),
///         1 = B(u16),
///         2 = C(u32),
///     }
/// }
/// ```
#[macro_export]
macro_rules! rpc_enum {
    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident: $repr:ty {
            $($(#[$variant_meta:meta])* $code:literal = $variant:ident($ty:ty),)*
        }
    ) => {paste::paste! {
        $vis use self::[<$name:snake _impl>]::{$name, [<$name Resolver>], [<Archived $name>]};

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
