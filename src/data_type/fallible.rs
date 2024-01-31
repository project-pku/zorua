use crate::traits::{BackingBitField, BackingField, ZoruaBitField, ZoruaField};
use std::{
    fmt::{Debug, Formatter},
    marker::PhantomData,
    mem::transmute_copy,
};

/// A wrapper type for enums that may have invalid discriminant values.
///
/// Used when reading data that might contain values outside the enum's valid range.
#[derive(Clone, Copy)]
pub struct Fallible<T: ZoruaFallible<B>, B: Copy> {
    _marker: PhantomData<T>,
    pub value: B,
}

///# Safety
///
/// Conditions to be safely implemented:
/// - The generic type [B] must have the same size (but not necessarily [alignment](https://doc.rust-lang.org/std/mem/fn.transmute_copy.html)) as [Self].
/// - The [is_valid] function must be properly implemented, i.e. every instance of [B] that transmuted to [Self] is invalid, should return false.
pub unsafe trait ZoruaFallible<B: Copy> {
    fn is_valid(value: B) -> bool;
}

impl<T: ZoruaFallible<B>, B: Copy> Fallible<T, B> {
    pub fn from_value(value: T) -> Fallible<T, B> {
        Fallible {
            _marker: Default::default(),
            value: unsafe { transmute_copy(&value) },
        }
    }

    pub fn from_raw(value: B) -> Fallible<T, B> {
        Fallible {
            _marker: Default::default(),
            value,
        }
    }
}

impl<T: ZoruaFallible<B>, B: BackingField> ZoruaField for Fallible<T, B> {
    fn swap_bytes_mut(&mut self) {
        B::swap_bytes_mut(&mut self.value)
    }
}

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

impl<T: ZoruaFallible<B>, B: Copy> From<T> for Fallible<T, B> {
    fn from(value: T) -> Self {
        Self::from_value(value)
    }
}

impl<T: ZoruaFallible<B>, B: PartialEq + Copy> PartialEq for Fallible<T, B> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<T: ZoruaFallible<B>, B: Debug + Copy> Debug for Fallible<T, B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}
