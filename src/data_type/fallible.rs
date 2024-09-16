use crate::traits::{BackingBitField, BackingField, ZoruaBitField, ZoruaField};
use core::{marker::PhantomData, mem};

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
/// - The [ZoruaFallible::is_valid()] function must be properly implemented. That is to say,
///   it should return false for every instance of `B` that is invalid when transmuted to `Self`.
///
/// The [derive macro](zorua_macro::zoruafallible_derive_macro) for this trait ensures all of
/// these requirements, and also implements [TryInto]`<Self>` for [Fallible]`<Self, B>`,
/// so you should prefer using that.
pub unsafe trait ZoruaFallible<B> {
    fn is_valid(value: B) -> bool;
}

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
