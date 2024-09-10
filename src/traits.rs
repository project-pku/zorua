use std::{
    mem::{self, ManuallyDrop},
    num::{NonZeroU128, NonZeroU16, NonZeroU32, NonZeroU64, NonZeroU8},
};

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
    ("arbitrary", $backing:ty, $($ty:ty),*) => {
        $(
            impl ZoruaBitField for $ty {
                type BitRepr = Self;
                fn to_bit_repr(self) -> Self::BitRepr { self }
                fn from_bit_repr(value: Self::BitRepr) -> Self { value }
            }
            impl BackingBitField for $ty {
                type ByteRepr = $backing;
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

/// Automates boilerplate for implementing ZoruaField
/// on unsigned Option<NonZero*> types
macro_rules! impl_nonzero_zorua_field {
    ($(($ty:ty, $ty2:ty)),*) => {
        $(
            #[allow(clippy::missing_transmute_annotations)] //needed to be generic over u8/u16/u32/u64
            unsafe impl ZoruaField for Option<$ty> {
                fn swap_bytes_mut(&mut self) {
                    let x: $ty2 = unsafe { mem::transmute((*self)) };
                    *self = unsafe { mem::transmute(x.swap_bytes()) };
                }
            }
        )*
    };
}

/// Automates boilerplate for implementing ZoruaField
/// on tuples of ZoruaFields
macro_rules! impl_zorua_field_for_tuple {
    // Base case: single item tuple
    ($($T:ident),*) => {
        #[allow(non_snake_case)]
        unsafe impl<$($T: ZoruaField),*> ZoruaField for ($($T,)*) {
                    fn swap_bytes_mut(&mut self) {
                let ($($T,)*) = self;
                $(
                    $T.swap_bytes_mut();
                )*
            }
        }
    };
}

//------------- Transmutable trait + impls -------------
/// # Safety
/// This trait should only be implemented by types that intened to be
/// transmutable to/from a type that implements [ZoruaField].
pub unsafe trait Transmutable {
    const HAS_PADDING: bool;
}
unsafe impl<T: ZoruaField> Transmutable for T {
    const HAS_PADDING: bool = false;
}
unsafe impl<const ALIGN: usize, const SIZE: usize> Transmutable for AlignedBytes<ALIGN, SIZE>
where
    Align<ALIGN>: Alignment,
{
    const HAS_PADDING: bool = SIZE % ALIGN != 0;
}

//------------- Field trait + impls -------------
/// const assertion that ensures that target has a known
/// endianness, so that the [ZoruaField] trait is well-formed.
const _: () = assert!(
    cfg!(target_endian = "big") || cfg!(target_endian = "little"),
    "This crate can only be compiled on little or big endian systems"
);

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

    fn as_bytes_ref(&self) -> &[u8] {
        let len = std::mem::size_of_val(self);
        let slf: *const Self = self;
        unsafe { std::slice::from_raw_parts(slf.cast::<u8>(), len) }
    }

    fn as_bytes_mut(&mut self) -> &mut [u8] {
        let len = mem::size_of_val(self);
        let slf: *mut Self = self;
        unsafe { std::slice::from_raw_parts_mut(slf.cast::<u8>(), len) }
    }

    fn try_from_bytes_ref(bytes: &[u8]) -> Result<&Self, CastError> {
        if bytes.len() != mem::size_of::<Self>() {
            Err(CastError::SizeMismatch)
        } else if (bytes.as_ptr() as *const ()).align_offset(mem::align_of::<Self>()) != 0 {
            Err(CastError::AlignmentTooStrict)
        } else {
            Ok(unsafe { &*(bytes.as_ptr() as *const Self) })
        }
    }

    fn try_from_bytes_mut(bytes: &mut [u8]) -> Result<&mut Self, CastError> {
        if bytes.len() != mem::size_of::<Self>() {
            Err(CastError::SizeMismatch)
        } else if (bytes.as_ptr() as *const ()).align_offset(mem::align_of::<Self>()) != 0 {
            Err(CastError::AlignmentTooStrict)
        } else {
            Ok(unsafe { &mut *(bytes.as_mut_ptr() as *mut Self) })
        }
    }

    /// Transmutes `self` into a different [`ZoruaField`] type `T`.
    /// This is useful for transmuting between dependently-sized `ZouraFields`
    /// where the caller can assure the equality of the two types' sizes.
    ///
    /// Unlike the [`macro@crate::transmute`] macro, this function can
    /// only transmute between `ZoruaField`s, and does not check the sizes
    /// of the types (hence the `unsafe` marker).
    ///
    /// # Safety
    /// The caller must ensure that the sizes of `Self` and `T` are equal.
    unsafe fn transmute_size_blind<T: ZoruaField>(self) -> T {
        unsafe {
            // Prevent `a` from being dropped
            let mut a = ManuallyDrop::new(self);

            // Cast the pointer of `a` to the pointer of the desired type
            let ptr = &mut *a as *mut Self as *mut T;

            // Read the value from the pointer
            ptr.read()
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
impl_nonzero_zorua_field!(
    (NonZeroU8, u8),
    (NonZeroU16, u16),
    (NonZeroU32, u32),
    (NonZeroU64, u64),
    (NonZeroU128, u128)
);

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

impl_zorua_field_for_tuple!(T, U);
impl_zorua_field_for_tuple!(T, U, V);
impl_zorua_field_for_tuple!(T, U, V, W);
impl_zorua_field_for_tuple!(T, U, V, W, X);
impl_zorua_field_for_tuple!(T, U, V, W, X, Y);
impl_zorua_field_for_tuple!(T, U, V, W, X, Y, Z);

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

impl_bit_backing!("arbitrary", u8, u1, u2, u3, u4, u5, u6, u7);
impl_bit_backing!("arbitrary", u16, u9, u10, u11, u12, u13, u14, u15);
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
