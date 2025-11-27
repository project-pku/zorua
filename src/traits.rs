use core::mem;

use crate::data_type::*;

pub use zorua_macro::*;

/// Automates boilerplate for implementing ZoruaField
/// and BackingField on endian-aware int types
macro_rules! impl_backing_endian {
    ($(($ty_e:tt, $ty:ty)),*) => {
        $(
            impl<E: Endian> BackingField for $ty_e<E> {
                type Primitive = $ty;

                fn get_bits_at<T: BackingBitField>(self, index: usize) -> T
                where Self: From<T::ByteRepr> + TryInto<T::ByteRepr> {
                    let masked_value = (self.value()
                        & (Self::from(T::MASK).value() << index)) >> index;
                    T::from_backed(
                        Self::new(masked_value).try_into().unwrap_or_else(
                            |_| {
                                panic!("Zorua Error: The BitRepr::MASK of type {} must be wrong",
                                    core::any::type_name::<$ty_e<E>>())
                            }
                        )
                    )
                }
                fn set_bits_at<T: BackingBitField>(&mut self, value: T, index: usize)
                where Self: From<T::ByteRepr> + TryInto<T::ByteRepr> {
                    let mut native = self.value();
                    let mask_native = Self::from(T::MASK).value();
                    let value_native = Self::from(value.to_backed()).value();
                    native &= !(mask_native << index);
                    native |= value_native << index;
                    *self = Self::new(native);
                }
            }
        )*
    };
}

/// Automates boilerplate for implementing ZoruaBitField
/// and BackingBitField on arbitrary-int & endian-aware int types
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
                const MASK: Self::ByteRepr = (1 << <$ty>::BITS) - 1;
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

/// Compile-time assertion that `A` can be safely transmuted to `B`.
///
/// Checks:
/// - Sizes are equal
/// - Alignment of `A` is >= alignment of `B`
const fn assert_transmute_compatible<A, B>() {
    assert!(
        mem::size_of::<A>() == mem::size_of::<B>(),
        "transmute: size mismatch"
    );
    assert!(
        mem::align_of::<A>() >= mem::align_of::<B>(),
        "transmute: alignment mismatch"
    );
}

/// # Safety
/// This trait is safe to implement *iff*:
/// - `Self` a *POD*, which is to say any possible bit pattern produces a valid instance of it.
/// - The representation of `Self` is independent of the endianness of the system.
pub unsafe trait ZoruaField: Sized {
    fn as_bytes(&self) -> &[u8] {
        let slf: *const Self = self;
        unsafe { core::slice::from_raw_parts(slf.cast::<u8>(), mem::size_of::<Self>()) }
    }

    fn as_bytes_mut(&mut self) -> &mut [u8] {
        let slf: *mut Self = self;
        unsafe { core::slice::from_raw_parts_mut(slf.cast::<u8>(), mem::size_of::<Self>()) }
    }

    /// Transmutes a [`ZoruaField`] into another with a compatible layout.
    fn transmute<T: ZoruaField>(self) -> T {
        const { assert_transmute_compatible::<Self, T>() };
        // SAFETY: Layout compatibility verified by const assertion above
        unsafe {
            let result = core::ptr::read(&self as *const Self as *const T);
            core::mem::forget(self);
            result
        }
    }

    #[cfg(feature = "std")]
    fn box_transmute<T: ZoruaField>(self: Box<Self>) -> Box<T> {
        const { assert_transmute_compatible::<Self, T>() };
        unsafe {
            let raw_ptr: *mut Self = Box::into_raw(self);
            Box::from_raw(raw_ptr as *mut T)
        }
    }

    fn transmute_ref<T: ZoruaField>(&self) -> &T {
        const { assert_transmute_compatible::<Self, T>() };
        unsafe { mem::transmute(self) }
    }

    fn transmute_mut<T: ZoruaField>(&mut self) -> &mut T {
        const { assert_transmute_compatible::<Self, T>() };
        unsafe { mem::transmute(self) }
    }

    fn transmute_split_ref<A: ZoruaField, B: ZoruaField>(&self) -> (&A, &B) {
        const { assert_transmute_compatible::<Self, (A, B)>() };
        unsafe {
            let base = self as *const Self as *const u8;
            let a_ptr = base as *const A;
            let b_ptr = base.add(mem::size_of::<A>()) as *const B;
            (&*a_ptr, &*b_ptr)
        }
    }

    fn transmute_split_mut<A: ZoruaField, B: ZoruaField>(&mut self) -> (&mut A, &mut B) {
        const { assert_transmute_compatible::<Self, (A, B)>() };
        unsafe {
            let base = self as *mut Self as *mut u8;
            let a_ptr = base as *mut A;
            let b_ptr = base.add(mem::size_of::<A>()) as *mut B;
            (&mut *a_ptr, &mut *b_ptr)
        }
    }
}

/// A special kind of [ZoruaField] that can house [ZoruaBitField]s.
///
/// (Practically speaking, just the built-in uints.)
pub trait BackingField: ZoruaField + Copy + core::fmt::Debug + PartialEq {
    type Primitive;

    fn get_bits_at<T: BackingBitField>(self, index: usize) -> T
    where
        Self: From<T::ByteRepr> + TryInto<T::ByteRepr>;

    fn set_bits_at<T: BackingBitField>(&mut self, value: T, index: usize)
    where
        Self: From<T::ByteRepr> + TryInto<T::ByteRepr>;
}

impl BackingField for u8 {
    type Primitive = u8;

    fn get_bits_at<T: BackingBitField>(self, index: usize) -> T
    where
        Self: From<T::ByteRepr> + TryInto<T::ByteRepr>,
    {
        T::from_backed(
            ((self & (Self::from(T::MASK) << index)) >> index)
                .try_into()
                .unwrap_or_else(|_| {
                    panic!(
                        "Zorua Error: The BitRepr::MASK of type {} must be wrong",
                        core::any::type_name::<u8>()
                    )
                }),
        )
    }
    fn set_bits_at<T: BackingBitField>(&mut self, value: T, index: usize)
    where
        Self: From<T::ByteRepr> + TryInto<T::ByteRepr>,
    {
        *self &= !(Self::from(T::MASK) << index);
        *self |= Self::from(value.to_backed()) << index;
    }
}
unsafe impl ZoruaField for u8 {}

impl_backing_endian!((U16, u16), (U32, u32), (U64, u64));

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
    type ByteRepr; //Smallest primitive int that can represent this type
    const MASK: Self::ByteRepr;
    fn to_backed(self) -> Self::ByteRepr;
    fn from_backed(value: Self::ByteRepr) -> Self;
}

impl_bit_backing!("arbitrary", u8, u1, u2, u3, u4, u5, u6, u7);
impl_bit_backing!("arbitrary", u16, u9, u10, u11, u12, u13, u14, u15);
impl_bit_backing!("native", u8, u16, u32, u64);

impl ZoruaBitField for bool {
    type BitRepr = u1;
    fn to_bit_repr(self) -> Self::BitRepr {
        u1::new(if self { 1 } else { 0 })
    }
    fn from_bit_repr(value: Self::BitRepr) -> Self {
        value == u1::MAX
    }
}
