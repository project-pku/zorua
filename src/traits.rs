use std::mem;

use crate::data_type::*;

pub use zorua_macro::*;

/// Automates boilerplate for implementing ZoruaField
/// and BackingField on built-in int types
macro_rules! impl_backing {
    ($($ty:ty),*) => {
        $(
            impl BackingField for $ty {
                fn get_bits_at<T: BackingBitField>(self, index: usize) -> T
                where Self: From<T::ByteRepr> + TryInto<T::ByteRepr> {
                    T::from_backed(
                        ((self & (Into::<$ty>::into(T::MASK) << index)) >> index).try_into().unwrap_or_else(
                            |_| {
                                panic!("Zorua Error: The BitRepr::MASK of type {} must be wrong",
                                    std::any::type_name::<$ty>())
                            }
                        )
                    )
                }
                fn set_bits_at<T: BackingBitField>(&mut self, value: T, index: usize)
                where Self: From<T::ByteRepr> + TryInto<T::ByteRepr> {
                    *self &= !(Into::<$ty>::into(T::MASK) << index);
                    *self |= (Into::<$ty>::into(value.to_backed())) << index;
                }
            }
            unsafe impl ZoruaField for $ty {
                fn swap_bytes_mut(&mut self) {
                    *self = (*self).swap_bytes();
                }
            }
        )*
    };
}

/// Automates boilerplate for implementing ZoruaBitField
/// and BackingBitField on arbitrary-int & built-in int types
macro_rules! impl_bit_backing {
    ("arbitrary_u8", $($ty:ty),*) => {
        $(
            impl ZoruaBitField for $ty {
                type BitRepr = Self;
                fn to_bit_repr(self) -> Self::BitRepr { self }
                fn from_bit_repr(value: Self::BitRepr) -> Self { value }
            }
            impl BackingBitField for $ty {
                type ByteRepr = u8;
                const MASK: Self::ByteRepr = unsafe { std::mem::transmute(<$ty>::MAX) };
                fn to_backed(self) -> Self::ByteRepr {
                    self.into()
                }
                fn from_backed(value: Self::ByteRepr) -> Self {
                    Self::new(value)
                }
            }
        )*
    };
    ("native", $($ty:ty),*) => {
        $(
            impl ZoruaBitField for $ty {
                type BitRepr = Self;
                fn to_bit_repr(self) -> Self::BitRepr { self }
                fn from_bit_repr(value: Self::BitRepr) -> Self { value }
            }
            impl BackingBitField for $ty {
                type ByteRepr = Self;
                const MASK: Self::ByteRepr = Self::MAX;
                fn to_backed(self) -> Self::ByteRepr { self }
                fn from_backed(value: Self::ByteRepr) -> Self { value }
            }
        )*
    };
}

//------------- Field trait + impls -------------
/// const assertion that ensures that target has a known
/// endianness, so that the [ZoruaField] trait is well-formed.
const _: () = assert!(
    cfg!(target_endian = "big") || cfg!(target_endian = "little"),
    "This crate can only be compiled on little or big endian systems"
);

pub enum Endian {
    Native,
    Little,
    Big,
}

#[derive(Debug)]
pub enum CastError {
    /// Denotes that the size of the target type and the byte slice.
    SizeMismatch,

    /// Denotes that the alignment of the target type was stricter than the byte slice.
    AlignmentTooStrict,
}

/// # Safety
/// This trait is safe to implement *iff*:
/// - `Self` a *POD*, which is to say any possible bit pattern produces a valid instance of it.
/// - The [ZoruaField::swap_bytes_mut] method is properly implemented. For primitive types it
/// simply swaps the byte order, for composite types it swaps the byte order of all its fields.
pub unsafe trait ZoruaField: Sized {
    /// Swaps the byte order of self in-place.
    fn swap_bytes_mut(&mut self);

    /// Swaps the byte order of self in-place iff the target is big endian
    /// (is a no-op on little endian targets).
    fn to_le_mut(&mut self) {
        #[cfg(target_endian = "big")]
        {
            self.swap_bytes_mut();
        }
    }

    /// Swaps the byte order of self in-place iff the target is little endian
    /// (is a no-op on big endian targets).
    fn to_be_mut(&mut self) {
        #[cfg(target_endian = "little")]
        {
            self.swap_bytes_mut();
        }
    }

    /// Casts a `&Self` as a `&[u8]` without any further transformation.
    ///
    /// This is as opposed to [ZoruaField::as_bytes_mut], which *may* swap the
    /// byte order of &`Self` before casting, making it suitable for deserialization.
    fn as_bytes_ref(&self) -> &[u8] {
        let len = std::mem::size_of_val(self);
        let slf: *const Self = self;
        unsafe { std::slice::from_raw_parts(slf.cast::<u8>(), len) }
    }

    fn as_bytes_mut(&mut self, endian: Endian) -> &mut [u8] {
        match endian {
            Endian::Little => self.to_le_mut(),
            Endian::Big => self.to_be_mut(),
            Endian::Native => (),
        }
        let len = mem::size_of_val(self);
        let slf: *mut Self = self;
        unsafe { std::slice::from_raw_parts_mut(slf.cast::<u8>(), len) }
    }

    /// Attempts to cast a byte slice to a &`Self` without any further transformation.
    /// Provides a [CastError] if the cast failed.
    ///
    /// This is as opposed to [ZoruaField::try_from_bytes_mut], which *may* swap the
    /// byte order of &`Self` after casting, making it suitable for deserialization.
    fn try_from_bytes_ref(bytes: &[u8]) -> Result<&Self, CastError> {
        if bytes.len() != mem::size_of::<Self>() {
            Err(CastError::SizeMismatch)
        } else if (bytes.as_ptr() as *const ()).align_offset(mem::align_of::<Self>()) != 0 {
            Err(CastError::AlignmentTooStrict)
        } else {
            Ok(unsafe { &*(bytes.as_ptr() as *const Self) })
        }
    }

    fn try_from_bytes_mut(bytes: &mut [u8], endian: Endian) -> Result<&mut Self, CastError> {
        if bytes.len() != mem::size_of::<Self>() {
            Err(CastError::SizeMismatch)
        } else if (bytes.as_ptr() as *const ()).align_offset(mem::align_of::<Self>()) != 0 {
            Err(CastError::AlignmentTooStrict)
        } else {
            let value = unsafe { &mut *(bytes.as_mut_ptr() as *mut Self) };
            match endian {
                Endian::Little => value.to_le_mut(),
                Endian::Big => value.to_be_mut(),
                Endian::Native => (),
            }
            Ok(value)
        }
    }
}

/// A special kind of [ZoruaField] that can house [ZoruaBitField]s.
///
/// (Practically speaking, just the built-in uints.)
pub trait BackingField: ZoruaField + Copy + std::fmt::Debug + PartialEq {
    fn get_bits_at<T: BackingBitField>(self, index: usize) -> T
    where
        Self: From<T::ByteRepr> + TryInto<T::ByteRepr>;

    fn set_bits_at<T: BackingBitField>(&mut self, value: T, index: usize)
    where
        Self: From<T::ByteRepr> + TryInto<T::ByteRepr>;
}

impl_backing!(u8, u16, u32, u64, u128);

unsafe impl ZoruaField for () {
    fn swap_bytes_mut(&mut self) {}
}

unsafe impl<const N: usize, T: ZoruaField> ZoruaField for [T; N] {
    fn swap_bytes_mut(&mut self) {
        self.iter_mut().for_each(|value| {
            value.swap_bytes_mut();
        });
    }
}

//----------- BitField trait + impls -----------
pub trait ZoruaBitField {
    /// The smallest bit representation of this type.
    type BitRepr: BackingBitField;

    /// Returns a copy of this type as its N bit [BitRepr][ZoruaBitField].
    fn to_bit_repr(self) -> Self::BitRepr;

    /// Packs this type into its N bit [BitRepr][ZoruaBitField].
    fn from_bit_repr(value: Self::BitRepr) -> Self;
}

pub trait BackingBitField: ZoruaBitField + Copy {
    type ByteRepr: BackingField;
    const MASK: Self::ByteRepr;
    fn to_backed(self) -> Self::ByteRepr;
    fn from_backed(value: Self::ByteRepr) -> Self;
}

impl_bit_backing!("arbitrary_u8", u1, u2, u3, u4, u5, u6, u7);
impl_bit_backing!("native", u8, u16, u32, u64, u128);

impl ZoruaBitField for bool {
    type BitRepr = u1;
    fn to_bit_repr(self) -> Self::BitRepr {
        u1::new(if self { 1 } else { 0 })
    }
    fn from_bit_repr(value: Self::BitRepr) -> Self {
        value == u1::MAX
    }
}
