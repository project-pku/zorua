use core::{
    mem,
    num::{NonZeroU128, NonZeroU16, NonZeroU32, NonZeroU64, NonZeroU8},
};

use crate::{aligned_bytes_of, data_type::*};

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
                                    core::any::type_name::<$ty>())
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
            unsafe impl ZoruaField for $ty {}
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
                const MASK: Self::ByteRepr = unsafe { core::mem::transmute(<$ty>::MAX) };
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
            unsafe impl ZoruaField for Option<$ty> {}
        )*
    };
}

/// Automates boilerplate for implementing ZoruaField
/// on tuples of ZoruaFields
macro_rules! impl_zorua_field_for_tuple {
    ($($T:ident),*) => {
        unsafe impl<$($T: ZoruaField),*> ZoruaField for ($($T,)*) {}
    };
}

//------------- Field trait + impls -------------
/// const assertion that ensures that target has a known
/// endianness, so that the [ZoruaField] trait is well-formed.
const _: () = assert!(
    cfg!(target_endian = "big") || cfg!(target_endian = "little"),
    "This crate can only be compiled on little or big endian systems"
);

pub const fn compatible_layout<T, U>() -> bool {
    mem::size_of::<T>() == mem::size_of::<U>() && mem::align_of::<T>() >= mem::align_of::<U>()
}

/// # Safety
/// This trait is safe to implement *iff*:
/// - `Self` a *POD*, which is to say any possible bit pattern produces a valid instance of it.
pub unsafe trait ZoruaField: Sized {
    fn as_bytes(&self) -> &[u8] {
        let slf: *const Self = self;
        unsafe { core::slice::from_raw_parts(slf.cast::<u8>(), mem::size_of::<Self>()) }
    }

    fn as_bytes_mut(&mut self) -> &mut [u8] {
        let slf: *mut Self = self;
        unsafe { core::slice::from_raw_parts_mut(slf.cast::<u8>(), mem::size_of::<Self>()) }
    }

    /// Transmutes a [`ZoruaField`] into another with a [`compatible_layout`].
    fn transmute<T: ZoruaField>(self) -> T
    where
        [(); compatible_layout::<Self, T>() as usize - 1]:,
    {
        unsafe { crate::unconditional_transmute(self) }
    }

    #[cfg(feature = "std")]
    fn box_transmute<T: ZoruaField>(self: Box<Self>) -> Box<T>
    where
        [(); compatible_layout::<Self, T>() as usize - 1]:,
    {
        unsafe {
            let foo_ptr: *mut Self = Box::into_raw(self);
            let bar_ptr: *mut T = foo_ptr as *mut T;
            Box::from_raw(bar_ptr)
        }
    }

    fn transmute_ref<T: ZoruaField>(&self) -> &T
    where
        [(); compatible_layout::<Self, T>() as usize - 1]:,
    {
        unsafe { mem::transmute(self) }
    }

    fn transmute_mut<T: ZoruaField>(&mut self) -> &mut T
    where
        [(); compatible_layout::<Self, T>() as usize - 1]:,
    {
        unsafe { mem::transmute(self) }
    }

    fn transmute_split_ref<A: ZoruaField, B: ZoruaField>(&self) -> (&A, &B)
    where
        [(); compatible_layout::<Self, (A, B)>() as usize - 1]:,
    {
        unsafe {
            let base = self as *const Self as *const u8;
            let a_ptr = base as *const A;
            let b_ptr = base.add(mem::size_of::<A>()) as *const B;
            (&*a_ptr, &*b_ptr)
        }
    }

    fn transmute_split_mut<A: ZoruaField, B: ZoruaField>(&mut self) -> (&mut A, &mut B)
    where
        [(); compatible_layout::<Self, (A, B)>() as usize - 1]:,
    {
        unsafe {
            let base = self as *mut Self as *mut u8;
            let a_ptr = base as *mut A;
            let b_ptr = base.add(mem::size_of::<A>()) as *mut B;
            (&mut *a_ptr, &mut *b_ptr)
        }
    }

    fn into_aligned_bytes(self) -> aligned_bytes_of!(Self)
    where
        AlignOf<Self>: Alignment,
    {
        unsafe { crate::unconditional_transmute(self) }
    }

    fn as_aligned_bytes(&self) -> &aligned_bytes_of!(Self)
    where
        AlignOf<Self>: Alignment,
    {
        unsafe { mem::transmute(self) }
    }

    fn as_aligned_bytes_mut(&mut self) -> &mut aligned_bytes_of!(Self)
    where
        AlignOf<Self>: Alignment,
    {
        unsafe { mem::transmute(self) }
    }
}

impl<T: ZoruaField> From<T> for aligned_bytes_of!(T)
where
    AlignOf<T>: Alignment,
{
    fn from(value: T) -> Self {
        value.into_aligned_bytes()
    }
}

/// A special kind of [ZoruaField] that can house [ZoruaBitField]s.
///
/// (Practically speaking, just the built-in uints.)
pub trait BackingField: ZoruaField + Copy + core::fmt::Debug + PartialEq {
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

unsafe impl ZoruaField for () {}

unsafe impl<const N: usize, T: ZoruaField> ZoruaField for [T; N] {}

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
