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

    pub const fn mirror_tag_i16(t: i16) -> i32 {
        let le = t.to_le_bytes();
        i32::from_le_bytes([le[0], le[1], le[1], le[0]])
    }

    pub const fn mirror_tag_u32(t: u32) -> u64 {
        let le = t.to_le_bytes();
        u64::from_le_bytes([le[0], le[1], le[2], le[3], le[3], le[2], le[1], le[0]])
    }

    pub const fn mirror_tag_i32(t: i32) -> i64 {
        let le = t.to_le_bytes();
        i64::from_le_bytes([le[0], le[1], le[2], le[3], le[3], le[2], le[1], le[0]])
    }

    pub const fn mirror_tag_u64(t: u64) -> u128 {
        let le = t.to_le_bytes();
        u128::from_le_bytes([
            le[0], le[1], le[2], le[3], le[4], le[5], le[6], le[7], le[7], le[6], le[5], le[4],
            le[3], le[2], le[1], le[0],
        ])
    }

    pub const fn mirror_tag_i64(t: i64) -> i128 {
        let le = t.to_le_bytes();
        i128::from_le_bytes([
            le[0], le[1], le[2], le[3], le[4], le[5], le[6], le[7], le[7], le[6], le[5], le[4],
            le[3], le[2], le[1], le[0],
        ])
    }
}

#[doc(hidden)]
#[macro_export] #[rustfmt::skip]
macro_rules! __rkyv_rpc_helper {
    (@REPR i8 $(#[$meta:meta])* $vis:vis enum $name:ident { $($tt:tt)* } ) => {
        $(#[$meta])* #[repr(i8, align(1))] $vis enum $name { $($tt)* }
    };
    (@REPR i16 $(#[$meta:meta])* $vis:vis enum $name:ident { $($tt:tt)* } ) => {
        $(#[$meta])* #[repr(i32, align(4))] $vis enum $name { $($tt)* }
    };
    (@REPR i32 $(#[$meta:meta])* $vis:vis enum $name:ident { $($tt:tt)* } ) => {
        $(#[$meta])* #[repr(i64, align(8))] $vis enum $name { $($tt)* }
    };
    (@REPR i64 $(#[$meta:meta])* $vis:vis enum $name:ident { $($tt:tt)* } ) => {
        $(#[$meta])* #[repr(i128, align(8))] $vis enum $name { $($tt)* }
    };

    (@REPR u8 $(#[$meta:meta])* $vis:vis enum $name:ident { $($tt:tt)* } ) => {
        $(#[$meta])* #[repr(u8, align(1))] $vis enum $name { $($tt)* }
    };
    (@REPR u16 $(#[$meta:meta])* $vis:vis enum $name:ident { $($tt:tt)* } ) => {
        $(#[$meta])* #[repr(u32, align(4))] $vis enum $name { $($tt)* }
    };
    (@REPR u32 $(#[$meta:meta])* $vis:vis enum $name:ident { $($tt:tt)* } ) => {
        $(#[$meta])* #[repr(u64, align(8))] $vis enum $name { $($tt)* }
    };
    (@REPR u64 $(#[$meta:meta])* $vis:vis enum $name:ident { $($tt:tt)* } ) => {
        $(#[$meta])* #[repr(u128, align(8))] $vis enum $name { $($tt)* }
    };

    (@MIRROR i8 $tag:literal) => { $tag };
    (@MIRROR i16 $tag:literal) => { $crate::private::mirror_tag_i16($tag) };
    (@MIRROR i32 $tag:literal) => { $crate::private::mirror_tag_i32($tag) };
    (@MIRROR i64 $tag:literal) => { $crate::private::mirror_tag_i64($tag) };

    (@MIRROR u8 $tag:literal) => { $tag };
    (@MIRROR u16 $tag:literal) => { $crate::private::mirror_tag_u16($tag) };
    (@MIRROR u32 $tag:literal) => { $crate::private::mirror_tag_u32($tag) };
    (@MIRROR u64 $tag:literal) => { $crate::private::mirror_tag_u64($tag) };

    (@CAST i8) => { i8 };
    (@CAST i16) => { i16 };
    (@CAST i32) => { i64 };
    (@CAST i64) => { i128 };

    (@CAST u8) => { u8 };
    (@CAST u16) => { u16 };
    (@CAST u32) => { u64 };
    (@CAST u64) => { u128 };

    (@TO_NATIVE u8 => $value:expr)      => { $value };
    (@TO_NATIVE i8 => $value:expr)      => { $value };
    (@TO_NATIVE $ty:ty => $value:expr)  => { $value.to_native() };

    (@FROM_NATIVE u8 => $value:expr)      => { $value };
    (@FROM_NATIVE i8 => $value:expr)      => { $value };
    (@FROM_NATIVE $ty:ty => $value:expr)  => { <Archived<$ty>>::from_native($value) };
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
///
/// To box all variant values a shorthand syntax `Wrapper<Name>` is provided:
/// ```rust
/// rkyv_rpc::tuple_enum! {
///    pub enum Box<Example>: u16 {
///       1000 = A(u8),     // turned into `Box<u8>`
///       1001 = B(u16),    // turned into `Box<u16>`
///       1002 = C(String), // turned into `Box<String>`
///    }
/// }
/// ```
///
/// The `Wrapper` type, referred to by name instead of type, must implement `From` for all variant types.
#[macro_export]
macro_rules! tuple_enum {
    (
        $(#[$meta:meta])*
        $vis:vis enum $wrap:ident<$name:ident>: $repr:ty {
            $($(#[$variant_meta:meta])* $code:literal = $variant:ident($ty:ty)),* $(,)?
        }
    ) => {
        $crate::tuple_enum! {
            $(#[$meta])*
            $vis enum $name: $repr {
                $($(#[$variant_meta])* $code = $variant($wrap<$ty>)),*
            }
        }

        $(
            impl From<$ty> for $name {
                fn from(value: $ty) -> Self {
                    $name::$variant($wrap::from(value))
                }
            }
        )*
    };

    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident: $repr:ty {
            $($(#[$variant_meta:meta])* $code:literal = $variant:ident($ty:ty)),* $(,)?
        }
    ) => {$crate::paste::paste! {
        $vis use [<$name:snake _impl>]::{$name, [<$name Resolver>], [<Archived $name>]};

        mod [<$name:snake _impl>] {
            #[allow(unused_imports)]
            use super::*; // import any types needed

            $(#[$meta])*
            #[repr($repr)]
            pub enum $name {
                $($(#[$variant_meta])* $variant($ty) = $code,)*
            }

            use $crate::rkyv::{Archive, Archived, Serialize, Deserialize, Place, Portable, Resolver};
            use $crate::rkyv::bytecheck::{CheckBytes, UnnamedEnumVariantCheckContext, InvalidEnumDiscriminantError};
            use $crate::rkyv::rancor::{Fallible, Trace, Source};

            $(
                impl From<$ty> for $name {
                    fn from(value: $ty) -> Self {
                        Self::$variant(value)
                    }
                }

                #[repr(C)]
                struct [<Archived $variant Variant>] {
                    tag: ArchivedTag,
                    data: Archived<$ty>,
                    mkr: ::core::marker::PhantomData<$name>,
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
                    $($variant(Archived<$ty>) = ArchivedTag::$variant as _,)*
                }
            }

            // SAFETY: This is safe because the tags are mirrored and endianness is preserved
            unsafe impl Portable for [<Archived $name>] {}

            #[repr($repr)]
            pub enum [<$name Resolver>] {
                $($variant(Resolver<$ty>) = $code,)*
            }

            impl Archive for $name {
                type Archived = [<Archived $name>];
                type Resolver = [<$name Resolver>];

                fn resolve(&self, resolver: Self::Resolver, out: Place<Self::Archived>) {
                    match resolver {$(
                        [<$name Resolver>]::$variant(resolver_0) => match self {
                            $name::$variant(self_0) => {
                                <$ty as Archive>::resolve(self_0, resolver_0, unsafe {
                                    let out = out.cast_unchecked::<[<Archived $variant Variant>]>();
                                    ::core::ptr::addr_of_mut!((*out.ptr()).tag).write(ArchivedTag::$variant);
                                    Place::from_field_unchecked(out, ::core::ptr::addr_of_mut!((*out.ptr()).data))
                                });
                            },

                            #[allow(unreachable_patterns)]
                            _ => unsafe { ::core::hint::unreachable_unchecked() },
                        },
                    )*}
                }
            }

            impl<S: Fallible + ?Sized> Serialize<S> for $name
                where $($ty: Serialize<S>,)*
            {
                fn serialize(&self, serializer: &mut S) -> Result<[<$name Resolver>], S::Error> {
                    Ok(match self {
                        $($name::$variant(data) => [<$name Resolver>]::$variant(Serialize::serialize(data, serializer)?),)*
                    })
                }
            }

            impl<D: Fallible + ?Sized> Deserialize<$name, D> for [<Archived $name>]
                where $(Archived<$ty>: Deserialize<$ty, D>,)*
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
                $(Archived<$ty>: CheckBytes<C>,)*
            {
                unsafe fn check_bytes(value: *const Self, context: &mut C) -> Result<(), <C as Fallible>::Error> {
                    Ok(match *value.cast::<$crate::__rkyv_rpc_helper!(@CAST $repr)>() {
                        $(ArchivedTag::[<$variant Discriminant>] => {
                            let value = value.cast::<[<Archived $variant Variant>]>();
                            CheckBytes::check_bytes(::core::ptr::addr_of!((*value).data), context).map_err(|e| {
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
/// This macro automatically derives `Debug`, `Clone`, `Copy`, `PartialEq`, `Eq`, and `Hash` for the enum.
///
/// `#[repr(u8)]` and `#[repr(i8)]` are passed through without creating a mirror type, making them
/// easier to work with at the mental overhead cost of not creating a dedicated archived type.
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
///     pub enum Example2: u16 {
///         1000 = A,
///         1001 = B,
///         1002 = C,
///     }
/// }
/// ```
#[macro_export]
macro_rules! unit_enum {
    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident: u8 {
            $($(#[$variant_meta:meta])* $code:literal = $variant:ident),* $(,)?
        }
    ) => {
        $crate::unit_enum! { @SINGLE_BYTE $(#[$meta])* $vis enum $name: u8 { $($(#[$variant_meta])* $code = $variant),* } }
    };

    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident: i8 {
            $($(#[$variant_meta:meta])* $code:literal = $variant:ident),* $(,)?
        }
    ) => {
        $crate::unit_enum! { @SINGLE_BYTE $(#[$meta])* $vis enum $name: i8 { $($(#[$variant_meta])* $code = $variant),* } }
    };

    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident: $repr:ty {
            $($(#[$variant_meta:meta])* $code:literal = $variant:ident),* $(,)?
        }
    ) => {$crate::paste::paste! {
        $vis use [<$name:snake _impl>]::{$name, [<Archived $name>]};

        mod [<$name:snake _impl>] {
            $(#[$meta])*
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
            #[repr($repr)]
            pub enum $name {
                $($(#[$variant_meta])* $variant = $code,)*
            }

            use $crate::rkyv::{Archive, Serialize, Deserialize, Portable};
            use $crate::rkyv::place::Place;
            use $crate::rkyv::traits::{NoUndef, CopyOptimization};
            use $crate::rkyv::bytecheck::{CheckBytes, InvalidEnumDiscriminantError};
            use $crate::rkyv::rancor::{Fallible, Source};

            $crate::__rkyv_rpc_helper! {
                @REPR $repr
                #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
            unsafe impl NoUndef for [<Archived $name>] {}

            impl Archive for $name {
                type Archived = [<Archived $name>];
                type Resolver = ();

                // enable for u8/i8 reprs
                const COPY_OPTIMIZATION: CopyOptimization<Self> = unsafe {
                    CopyOptimization::enable_if(::core::mem::size_of::<$repr>() == 1)
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

            impl [<Archived $name>] {
                pub const fn get(&self) -> $name {
                    match *self {
                        $(<[<Archived $name>]>::$variant => $name::$variant,)*
                    }
                }
            }

            impl<D: Fallible + ?Sized> Deserialize<$name, D> for [<Archived $name>] {
                #[inline]
                fn deserialize(&self, _: &mut D) -> Result<$name, D::Error> {
                    Ok(self.get())
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

                        invalid_discriminant => return Err(<<C as Fallible>::Error as Source>::new(InvalidEnumDiscriminantError {
                            enum_name: stringify!([<Archived $name>]),
                            invalid_discriminant,
                        })),
                    })
                }
            }
        }
    }};

    (@SINGLE_BYTE
        $(#[$meta:meta])*
        $vis:vis enum $name:ident: $repr:ty {
            $($(#[$variant_meta:meta])* $code:literal = $variant:ident),* $(,)?
        }
    ) => {
        $(#[$meta])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #[repr(align(1), $repr)]
        $vis enum $name {
            $($(#[$variant_meta])* $variant = $code,)*
        }

        const _: () = {
            use $crate::rkyv::{Archive, Serialize, Deserialize, Portable};
            use $crate::rkyv::place::Place;
            use $crate::rkyv::traits::{NoUndef, CopyOptimization};
            use $crate::rkyv::bytecheck::{CheckBytes, InvalidEnumDiscriminantError};
            use $crate::rkyv::rancor::{Fallible, Source};

            unsafe impl Portable for $name {}
            unsafe impl NoUndef for $name {}

            impl Archive for $name {
                type Archived = $name;
                type Resolver = ();

                const COPY_OPTIMIZATION: CopyOptimization<Self> = unsafe { CopyOptimization::enable() };

                #[inline]
                fn resolve(&self, _: Self::Resolver, out: Place<Self::Archived>) {
                    out.write(*self);
                }
            }

            impl<S: Fallible + ?Sized> Serialize<S> for $name {
                #[inline]
                fn serialize(&self, _: &mut S) -> Result<(), S::Error> {
                    Ok(())
                }
            }

            impl<D: Fallible + ?Sized> Deserialize<$name, D> for $name {
                #[inline]
                fn deserialize(&self, _: &mut D) -> Result<$name, D::Error> {
                    Ok(*self)
                }
            }

            unsafe impl<C> CheckBytes<C> for $name
            where
                C: Fallible + ?Sized,
                <C as Fallible>::Error: Source,
            {
                #[inline]
                unsafe fn check_bytes(value: *const Self, _: &mut C) -> Result<(), <C as Fallible>::Error> {
                    Ok(match *value.cast::<$repr>() {
                        $($code => (),)*

                        invalid_discriminant => return Err(<<C as Fallible>::Error as Source>::new(InvalidEnumDiscriminantError {
                            enum_name: stringify!($name),
                            invalid_discriminant,
                        })),
                    })
                }
            }
        };
    };
}

/// Similar to [`unit_enum!`], but represents a unit enum as its tag directly. For example a
/// `#[repr(u16)]` enum will be represented as an `Archived<u16>` in the archive, which
/// will be endianness-agnostic.
///
/// The downside of this is that the enum is not represented as another enum for the archived
/// type and must be manually converted to the original enum type via a `get` method.
#[macro_export]
macro_rules! enum_codes {
    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident: $archived_vis:vis $repr:ty $(= $unknown:ident)? {
            $($(#[$variant_meta:meta])* $code:literal = $variant:ident),* $(,)?
        }
    ) => {$crate::paste::paste! {
        $vis use [<$name:snake _impl>]::{$name, [<Archived $name>]};

        mod [<$name:snake _impl>] {
            $(#[$meta])*
            #[repr($repr)]
            pub enum $name {
                $($(#[$variant_meta])* $variant = $code,)*
            }

            use $crate::rkyv::{Archive, Archived, Serialize, Deserialize, Portable};
            use $crate::rkyv::place::Place;
            use $crate::rkyv::traits::{NoUndef, CopyOptimization};
            use $crate::rkyv::bytecheck::{CheckBytes, InvalidEnumDiscriminantError};
            use $crate::rkyv::rancor::{Fallible, Source};

            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            #[repr(transparent)]
            pub struct [<Archived $name>]($archived_vis Archived<$repr>);

            #[allow(non_upper_case_globals)]
            impl [<Archived $name>] {
                $(const [<$variant Tag>]: $repr = $code;)*

                #[allow(unreachable_patterns)]
                pub const fn get(&self) -> $name {
                    match $crate::__rkyv_rpc_helper!(@TO_NATIVE $repr => self.0) {
                        $(Self::[<$variant Tag>] => $name::$variant,)*
                        $(_ => $name::$unknown,)?

                        // SAFETY: Value is checked by `check_bytes` to be valid
                        _ => unsafe { ::core::hint::unreachable_unchecked() },
                    }
                }
            }

            unsafe impl Portable for [<Archived $name>] {}
            unsafe impl NoUndef for [<Archived $name>] {}

            impl Archive for $name {
                type Archived = [<Archived $name>];
                type Resolver = ();

                const COPY_OPTIMIZATION: CopyOptimization<Self> = unsafe {
                    CopyOptimization::enable_if(<$repr as Archive>::COPY_OPTIMIZATION.is_enabled())
                };

                #[inline]
                fn resolve(&self, _: Self::Resolver, out: Place<Self::Archived>) {
                    out.write([<Archived $name>]($crate::__rkyv_rpc_helper!(@FROM_NATIVE $repr => *self as $repr)));
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
                    Ok(self.get())
                }
            }

            unsafe impl<C> CheckBytes<C> for [<Archived $name>]
            where
                C: Fallible + ?Sized,
                <C as Fallible>::Error: Source,
            {
                #[allow(unreachable_patterns)]
                unsafe fn check_bytes(value: *const Self, _: &mut C) -> Result<(), <C as Fallible>::Error> {
                    Ok(match *value.cast::<$repr>() {
                        $([<Archived $name>]::[<$variant Tag>] => (),)*
                        $(_ => { _ = $name::$unknown; })?

                        invalid_discriminant => return Err(<<C as Fallible>::Error as Source>::new(InvalidEnumDiscriminantError {
                            enum_name: stringify!([<Archived $name>]),
                            invalid_discriminant,
                        })),
                    })
                }
            }
        }
    }}
}

#[macro_export]
macro_rules! impl_rkyv_for_1byte_pod {
    ($ty:ty) => {
        const _: () = {
            assert!(::core::mem::size_of::<$ty>() == 1);

            use $crate::rkyv::place::{Initialized, Place};
            use $crate::rkyv::rancor::Fallible;
            use $crate::rkyv::traits::CopyOptimization;
            use $crate::rkyv::{Archive, Deserialize, Serialize};

            unsafe impl Initialized for $ty {}

            impl Archive for $ty {
                type Archived = $ty;
                type Resolver = ();

                const COPY_OPTIMIZATION: CopyOptimization<Self> =
                    unsafe { CopyOptimization::enable() };

                #[inline]
                fn resolve(&self, _: Self::Resolver, out: Place<Self::Archived>) {
                    out.write(*self); // also asserts Copy
                }
            }

            impl<S: Fallible + ?Sized> Serialize<S> for $ty {
                #[inline]
                fn serialize(&self, _: &mut S) -> Result<(), S::Error> {
                    Ok(())
                }
            }

            impl<D: Fallible + ?Sized> Deserialize<$ty, D> for $ty {
                #[inline]
                fn deserialize(&self, _: &mut D) -> Result<$ty, D::Error> {
                    Ok(*self)
                }
            }
        };
    };
}

use rkyv::{
    de::{Pool, Unpool},
    rancor::{Error, Failure, Strategy},
    Archive, Archived, Deserialize,
};

/// Extension trait for [`Archived`] types to provide simpler deserialization methods.
pub trait DeserializeExt<T: Archive> {
    /// Deserialize the archived value with a simple deserialization strategy, which duplicates
    /// deserializations of the same shared pointer and throws away errors.
    ///
    /// Use this for simpler "POD"-like types that don't need to cache shared pointers.
    fn deserialize_simple(&self) -> Result<T, Failure>
    where
        Archived<T>: Deserialize<T, Strategy<Unpool, Failure>>;

    /// Deserialize the archived value with a full deserialization strategy, which caches shared
    /// pointers and returns full errors.
    ///
    /// Use this for more complex types that need to cache shared pointers.
    fn deserialize_full(&self) -> Result<T, Error>
    where
        Archived<T>: Deserialize<T, Strategy<Pool, Error>>;
}

impl<T: Archive> DeserializeExt<T> for Archived<T> {
    #[inline]
    fn deserialize_simple(&self) -> Result<T, Failure>
    where
        Archived<T>: Deserialize<T, Strategy<Unpool, Failure>>,
    {
        self.deserialize(Strategy::wrap(&mut Unpool))
    }

    fn deserialize_full(&self) -> Result<T, Error>
    where
        Archived<T>: Deserialize<T, Strategy<Pool, Error>>,
    {
        self.deserialize(Strategy::wrap(&mut Pool::new()))
    }
}

#[cfg(test)]
mod tests {
    use super::DeserializeExt;
    use rkyv::Archived;

    #[derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
    pub struct Test {
        x: String,
    }

    #[test]
    fn test_simple_de() {
        let test = Test {
            x: "hello".to_string(),
        };
        let bytes = rkyv::to_bytes::<rkyv::rancor::Error>(&test).unwrap();
        let archived = unsafe { rkyv::access_unchecked::<Archived<Test>>(&bytes) };
        let deserialized: Test = archived.deserialize_simple().unwrap();
        assert_eq!(test.x, deserialized.x);
    }
}
