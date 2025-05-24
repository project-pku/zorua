use core::{convert::TryInto, marker::PhantomData, mem};

use crate::traits::{BackingBitField, BackingField, ZoruaBitField, ZoruaField};

/// A wrapper type for enums that may have invalid discriminant values.
///
/// Used when reading data that might contain values outside the enum's valid range.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Fallible<T: ZoruaFallible<B>, B> {
    _marker: PhantomData<T>,
    pub value: B,
}

/// Implementing this trait on a c-like enum for some generic backing type `B`, will enable
/// to usage of [Fallible]`<Self, B>`.
///
/// # Safety
/// Conditions to be safely implemented:
/// - This should only be implemented on
///   [c-like enums](https://doc.rust-lang.org/rust-by-example/custom_types/enum/c_like.html).
/// - `B` should implement at least one of [BackingField] or [BackingBitField].
/// - The generic type `B` must have the same size (but not necessarily
///   [alignment](https://doc.rust-lang.org/std/mem/fn.transmute_copy.html)) as `Self`.
///
/// The [derive macro](zorua_macro::zoruafallible_derive_macro) for this trait ensures all of
/// these requirements, and also implements [TryInto]`<Self>` for [Fallible]`<Self, B>`,
/// so you should prefer using that.
pub unsafe trait ZoruaFallible<B> {}

impl<T: ZoruaFallible<B>, B> Fallible<T, B> {
    pub fn from_value(value: T) -> Fallible<T, B> {
        Fallible {
            _marker: Default::default(),
            value: unsafe { mem::transmute_copy(&value) },
        }
    }

    pub fn from_raw(value: B) -> Fallible<T, B> {
        Fallible {
            _marker: Default::default(),
            value,
        }
    }

    /// Attempts to convert this `Fallible` into the target enum type `T`.
    /// Returns `Ok(T)` if the value corresponds to a valid variant,
    /// or `Err(B)` with the raw backing value if not.
    pub fn into_result(self) -> Result<T, B>
    where
        Self: TryInto<T, Error = B>,
    {
        self.try_into()
    }
}

unsafe impl<T: ZoruaFallible<B>, B: BackingField> ZoruaField for Fallible<T, B> {}

impl<T: ZoruaFallible<B>, B: BackingBitField> ZoruaBitField for Fallible<T, B> {
    type BitRepr = B;

    fn to_bit_repr(self) -> B {
        self.value
    }
    fn from_bit_repr(value: B) -> Self {
        Fallible {
            _marker: Default::default(),
            value,
        }
    }
}

impl<T: ZoruaFallible<B>, B> From<T> for Fallible<T, B> {
    fn from(value: T) -> Self {
        Self::from_value(value)
    }
}
