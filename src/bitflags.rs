/// (Nearly) Drop-in replacement for the [`bitflags!`](crate::og_bitflags::bitflags!) macro that generates safe `rkyv` compatible bitflags.
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
        $vis:vis impl $BitFlags:ident: $T:ty {
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

    (@RKYV_ONLY $vis:vis $BitFlags:ident:$T:ty) => {$crate::paste::paste! {
        #[doc = "Archived version of [`" $BitFlags "`], automatically made endian-agnostic.\n"]
        #[doc = "When comparing with the native bitflags, the archived bitflags will truncate any extra bits."]
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        #[repr(transparent)]
        $vis struct [<Archived $BitFlags>](pub $crate::rkyv::Archived<$T>);

        const _: () = {
            use $crate::rkyv::{Archive, Archived, Serialize, Deserialize, Portable};
            use $crate::rkyv::place::Place;
            use $crate::rkyv::traits::{CopyOptimization, NoUndef};
            use $crate::rkyv::bytecheck::CheckBytes;
            use $crate::rkyv::rancor::{Fallible, Source};

            const fn assert_trivial<T: Portable + NoUndef>() {}
            assert_trivial::<Archived<$T>>();

            unsafe impl Portable for [<Archived $BitFlags>] {}
            unsafe impl NoUndef for [<Archived $BitFlags>] {}

            impl Archive for $BitFlags {
                type Archived = [<Archived $BitFlags>];
                type Resolver = ();

                const COPY_OPTIMIZATION: CopyOptimization<Self> = unsafe {
                    CopyOptimization::enable_if(<Archived<$T>>::COPY_OPTIMIZATION.is_enabled())
                };

                #[inline]
                fn resolve(&self, _: (), out: Place<Self::Archived>) {
                    out.write([<Archived $BitFlags>]::from_native(*self))
                }
            }

            impl<S: Fallible + ?Sized> Serialize<S> for $BitFlags {
                #[inline]
                fn serialize(&self, _: &mut S) -> Result<(), S::Error> {
                    Ok(())
                }
            }

            impl<D: Fallible + ?Sized> Deserialize<$BitFlags, D> for [<Archived $BitFlags>] {
                #[inline]
                fn deserialize(&self, _: &mut D) -> Result<$BitFlags, D::Error> {
                    Ok(self.to_native_truncate())
                }
            }

            unsafe impl<C> CheckBytes<C> for [<Archived $BitFlags>]
            where
                C: Fallible + ?Sized,
                <C as Fallible>::Error: Source,
            {
                #[inline]
                unsafe fn check_bytes(value: *const Self, ctx: &mut C) -> Result<(), <C as Fallible>::Error> {
                    <Archived<$T>>::check_bytes(value.cast(), ctx)
                }
            }

            impl From<[<Archived $BitFlags>]> for $BitFlags {
                #[inline]
                fn from(archived: [<Archived $BitFlags>]) -> Self {
                    archived.to_native_truncate()
                }
            }

            impl [<Archived $BitFlags>] {
                /// Converts this archived bitflags to the native bitflags type, returning `None` if any
                /// bits are set that are not part of the bitflags.
                ///
                #[doc = "See [`" $BitFlags "::from_bits`] for more information."]
                #[inline]
                pub const fn to_native(&self) -> Option<$BitFlags> {
                    $BitFlags::from_bits(self.bits())
                }

                /// Converts this archived bitflags to the native bitflags type, truncating any extra bits.
                ///
                #[doc = "See [`" $BitFlags "::from_bits_truncate`] for more information."]
                #[inline]
                pub const fn to_native_truncate(&self) -> $BitFlags {
                    $BitFlags::from_bits_truncate(self.bits())
                }

                /// Converts this archived bitflags to the native bitflags type, retaining all bits, even
                /// if they are not part of the bitflags.
                ///
                #[doc = "See [`" $BitFlags "::from_bits_retain`] for more information."]
                #[inline]
                pub const fn to_native_retain(&self) -> $BitFlags {
                    $BitFlags::from_bits_retain(self.bits())
                }

                /// Converts the native bitflags type to an archived bitflags.
                #[inline]
                pub const fn from_native(native: $BitFlags) -> Self {
                    Self($crate::__rkyv_rpc_helper!(@FROM_NATIVE $T => native.bits()))
                }

                /// Check if the archived bitflags contains all the given bitflags.
                ///
                #[doc = "See [`" $BitFlags "::contains`] for more information."]
                #[inline]
                pub const fn contains(&self, other: $BitFlags) -> bool {
                    self.to_native_retain().contains(other)
                }

                /// Check if the archived bitflags intersect with any of the given bitflags.
                ///
                #[doc = "See [`" $BitFlags "::intersects`] for more information."]
                #[inline]
                pub const fn intersects(&self, other: $BitFlags) -> bool {
                    self.to_native_retain().intersects(other)
                }

                /// Whether all bits in this flags value are unset.
                #[inline]
                pub const fn is_empty(&self) -> bool {
                    self.to_native_retain().is_empty()
                }

                /// Whether all known bits in this flags value are set.
                #[inline]
                pub const fn is_all(&self) -> bool {
                    self.to_native_retain().is_all()
                }

                /// Get the underlying raw bits value.
                ///
                /// Note that this may contain bits that are not part of the original bitflags,
                /// if serialized with a different version of the bitflags. Use [`to_native`](Self::to_native) or
                /// [`to_native_truncate`](Self::to_native_truncate) to convert to the native bitflags type
                /// without extra bits.
                #[inline]
                pub const fn bits(&self) -> $T {
                    $crate::__rkyv_rpc_helper!(@TO_NATIVE $T => self.0)
                }
            }

            impl PartialEq<$BitFlags> for [<Archived $BitFlags>] {
                #[inline]
                fn eq(&self, other: &$BitFlags) -> bool {
                    self.to_native_truncate() == *other
                }
            }

            impl PartialEq<[<Archived $BitFlags>]> for $BitFlags {
                #[inline]
                fn eq(&self, other: &[<Archived $BitFlags>]) -> bool {
                    *self == other.to_native_truncate()
                }
            }
        };
    }};

    () => {};
}
