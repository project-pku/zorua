use crate::traits::{BackingBitField, BackingField, ZoruaBitField, ZoruaField};
use std::fmt::{Debug, Formatter};

mod aligned;
pub use aligned::*;
pub use ux2::{u1, u2, u3, u4, u5, u6, u7};

pub trait ZoruaFallible: Copy + Debug {
    type ByteRepr: BackingField;
    type BitRepr: BackingBitField;

    fn is_valid(value: Self::ByteRepr) -> bool;
}

#[derive(Clone, Copy)]
pub union Fallible<T: ZoruaFallible> {
    pub as_enum: T,
    pub as_byte_repr: T::ByteRepr,
    pub as_bit_repr: T::BitRepr,
}

impl<T: ZoruaFallible> Fallible<T> {
    pub fn value_or_byte_repr(self) -> Result<T, T::ByteRepr> {
        if T::is_valid(unsafe { self.as_byte_repr }) {
            Ok(unsafe { self.as_enum })
        } else {
            Err(unsafe { self.as_byte_repr })
        }
    }
    pub fn value_or_bit_repr(self) -> Result<T, T::BitRepr> {
        if T::is_valid(unsafe { self.as_byte_repr }) {
            Ok(unsafe { self.as_enum })
        } else {
            Err(unsafe { self.as_bit_repr })
        }
    }
    pub fn as_byte_repr(self) -> T::ByteRepr {
        unsafe { self.as_byte_repr }
    }
    pub fn as_bit_repr(self) -> T::BitRepr {
        unsafe { self.as_bit_repr }
    }
}

impl<T: ZoruaFallible> ZoruaField for Fallible<T> {
    fn swap_bytes_mut(&mut self) {
        T::ByteRepr::swap_bytes_mut(unsafe { &mut self.as_byte_repr })
    }
}

impl<T: ZoruaFallible> ZoruaBitField for Fallible<T> {
    type BitRepr = T::BitRepr;
    fn to_bit_repr(self) -> Self::BitRepr {
        self.as_bit_repr()
    }
    fn from_bit_repr(value: Self::BitRepr) -> Self {
        Fallible { as_bit_repr: value }
    }
}

impl<T: ZoruaFallible> From<T> for Fallible<T> {
    fn from(value: T) -> Self {
        Fallible { as_enum: value }
    }
}

impl<T: ZoruaFallible + PartialEq> PartialEq for Fallible<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_byte_repr() == other.as_byte_repr()
    }
}

impl<T: ZoruaFallible + Debug> Debug for Fallible<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.value_or_byte_repr().fmt(f)
    }
}
