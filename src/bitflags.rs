/// Drop-in replacement for the [`bitflags!`](crate::og_bitflags::bitflags!) macro that generates safe `rkyv` compatible bitflags.
///
/// [Archive](rkyv::Archive), [Serialize](rkyv::Serialize), and [Deserialize](rkyv::Deserialize)
/// implementations are generated for the bitflags type.
///
/// Using the `@RKYV_ONLY` prefix in the macro will only generate the archived type and trait implementations,
/// skipping over the bitflags type generation.
///
/// # Example
/// ```rust
/// // use the same syntax as the original bitflags! macro
/// rkyv_rpc::bitflags! {
///     #[derive(Debug, Clone, Copy, PartialEq, Eq)]
///     pub struct ExampleBitflags: u16 {
///         const A = 0;
///         const B = 1;
///         const C = 2;
///     }
/// }
/// ```
#[macro_export]
macro_rules! bitflags {
    (
        $(#[$outer:meta])*
        $vis:vis struct $BitFlags:ident: $T:ty {
            $(
                $(#[$inner:ident $($args:tt)*])*
                const $Flag:tt = $value:expr;
            )*
        }

        $($t:tt)*
    ) => {
        $crate::og_bitflags::bitflags! {
            $(#[$outer])*
            $vis struct $BitFlags: $T {
                $(
                    $(#[$inner $($args)*])*
                    const $Flag = $value;
                )*
            }
        }

        $crate::bitflags!(@RKYV_ONLY $vis $BitFlags: $T);
        $crate::bitflags!($($t)*);
    };

    (
        $(#[$outer:meta])*
        impl $BitFlags:ident: $T:ty {
            $(
                $(#[$inner:ident $($args:tt)*])*
                const $Flag:tt = $value:expr;
            )*
        }

        $($t:tt)*
    ) => {
        $crate::og_bitflags::bitflags! {
            $(#[$outer])*
            impl $BitFlags: $T {
                $(
                    $(#[$inner $($args)*])*
                    const $Flag = $value;
                )*
            }
        }

        $crate::bitflags!(@RKYV_ONLY $vis $BitFlags: $T);
        $crate::bitflags!($($t)*);
    };

    (@RKYV_ONLY $vis:vis $name:ident:$ty:ty) => {$crate::paste::paste! {
        #[doc = "Archived version of [`" $name "`]."]
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        #[repr(transparent)]
        $vis struct [<Archived $name>](pub $crate::rkyv::Archived<$ty>);

        const _: () = {
            use $crate::rkyv::{Archive, Archived, Serialize, Deserialize, Portable};
            use $crate::rkyv::place::{Place, Initialized};
            use $crate::rkyv::traits::CopyOptimization;
            use $crate::rkyv::bytecheck::CheckBytes;
            use $crate::rkyv::rancor::{Fallible, Source};

            const fn assert_trivial<T: Portable + Initialized>() {}
            assert_trivial::<Archived<$ty>>();

            unsafe impl Portable for [<Archived $name>] {}
            unsafe impl Initialized for [<Archived $name>] {}

            impl Archive for $name {
                type Archived = [<Archived $name>];
                type Resolver = ();

                const COPY_OPTIMIZATION: CopyOptimization<Self> = unsafe {
                    CopyOptimization::enable_if(<Archived<$ty>>::COPY_OPTIMIZATION.is_enabled())
                };

                #[inline]
                fn resolve(&self, _: (), out: Place<Self::Archived>) {
                    out.write([<Archived $name>](<Archived<$ty>>::from_native(self.bits())))
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
                    Ok(self.to_native_truncate())
                }
            }

            unsafe impl<C> CheckBytes<C> for [<Archived $name>]
            where
                C: Fallible + ?Sized,
                <C as Fallible>::Error: Source,
            {
                #[inline]
                unsafe fn check_bytes(value: *const Self, ctx: &mut C) -> Result<(), <C as Fallible>::Error> {
                    <Archived<$ty>>::check_bytes(value.cast(), ctx)
                }
            }

            impl From<[<Archived $name>]> for $name {
                #[inline]
                fn from(archived: [<Archived $name>]) -> Self {
                    archived.to_native_truncate()
                }
            }

            impl [<Archived $name>] {
                /// Converts this archived bitflags to the native bitflags type, truncating any extra bits.
                ///
                #[doc = "See [`" $name "::from_bits_truncate`] for more information."]
                #[inline]
                pub const fn to_native_truncate(&self) -> $name {
                    $name::from_bits_truncate(self.0.to_native())
                }

                /// Converts this archived bitflags to the native bitflags type, retaining all bits, even
                /// if they are not part of the bitflags.
                ///
                #[doc = "See [`" $name "::from_bits_retain`] for more information."]
                #[inline]
                pub const fn to_native_retain(&self) -> $name {
                    $name::from_bits_retain(self.0.to_native())
                }

                /// Converts the native bitflags type to an archived bitflags.
                #[inline]
                pub const fn from_native(native: $name) -> Self {
                    Self(<Archived<$ty>>::from_native(native.bits()))
                }

                /// Check if the archived bitflags contains all the given bitflags.
                ///
                #[doc = "See [`" $name "::contains`] for more information."]
                #[inline]
                pub const fn contains(&self, other: $name) -> bool {
                    self.to_native_retain().contains(other)
                }

                /// Check if the archived bitflags intersect with any of the given bitflags.
                ///
                #[doc = "See [`" $name "::intersects`] for more information."]
                #[inline]
                pub const fn intersects(&self, other: $name) -> bool {
                    self.to_native_retain().intersects(other)
                }
            }
        };
    }};

    () => {};
}
