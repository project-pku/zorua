use core::mem;

use crate::data_type::*;

pub use zorua_macro::*;

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
        unsafe { mem::transmute(self) }
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

/// A special kind of [ZoruaField] that can be used as a backing storage for bitfields.
///
/// (Practically speaking, just the built-in uints like u8, u16, u32, u64.)
pub trait BackingField: ZoruaField + Copy + core::fmt::Debug + PartialEq {
    /// Get the bits at `index` as type `T`.
    ///
    /// This represents a bitfield access: `T` is the storage type of the subfield.
    fn get_bits_at<T: BackingStorage>(self, index: usize) -> T;

    /// Set the bits at `index` to the value of type `T`.
    ///
    /// This represents a bitfield modification.
    fn set_bits_at<T: BackingStorage>(&mut self, value: T, index: usize);
}

/// A marker trait for types that can represent a bitfield's value in storage.
///
/// This is implemented for the built-in uints and the [`u1`]...[`u15`] types.
pub trait BackingStorage: Copy {
    /// The bit mask for this type (as u64).
    const MASK: u64;

    /// The number of bits in this type.
    const BITS: usize;

    /// Convert to u64 representation.
    fn to_u64(self) -> u64;

    /// Convert from u64 representation.
    fn from_u64(value: u64) -> Self;

    /// The zero value for this type.
    const ZERO: Self;
}

impl BackingField for u8 {
    #[inline]
    fn get_bits_at<T: BackingStorage>(self, index: usize) -> T {
        T::from_u64((u64::from(self) & (T::MASK << index)) >> index)
    }

    #[inline]
    fn set_bits_at<T: BackingStorage>(&mut self, value: T, index: usize) {
        let val_native = value.to_u64();
        *self &= !((T::MASK as u8) << index);
        *self |= (val_native as u8) << index;
    }
}

// Implement BackingField for endian-aware types by delegating to their inner value
macro_rules! impl_backing_field_for_endian {
    ($($ty:ident),*) => {
        $(
            impl<E: Endian> BackingField for $ty<E> {
                #[inline]
                fn get_bits_at<T: BackingStorage>(self, index: usize) -> T {
                    let val = self.value() as u64;
                    T::from_u64((val & (T::MASK << index)) >> index)
                }

                #[inline]
                fn set_bits_at<T: BackingStorage>(&mut self, value: T, index: usize) {
                    let mut val = self.value() as u64;
                    val &= !(T::MASK << index);
                    val |= value.to_u64() << index;
                    *self = Self::new(val as _);
                }
            }
        )*
    };
}

impl_backing_field_for_endian!(U16, U32, U64);

// Implement BackingStorage for primitives and ux2 types
impl BackingStorage for bool {
    const MASK: u64 = 1;
    const BITS: usize = 1;
    #[inline]
    fn to_u64(self) -> u64 {
        self as u64
    }
    #[inline]
    fn from_u64(value: u64) -> Self {
        value != 0
    }
    const ZERO: Self = false;
}

macro_rules! impl_backing_storage_for_ux2 {
    ($($ty:ident),*) => {
        $(
            impl BackingStorage for $ty {
                const MASK: u64 = (1 << Self::BITS) - 1;
                const BITS: usize = Self::BITS as usize;
                #[inline]
                fn to_u64(self) -> u64 { self.into() }
                #[inline]
                fn from_u64(value: u64) -> Self { Self::new(value as _) }
                const ZERO: Self = Self::new(0);
            }
        )*
    };
}

impl_backing_storage_for_ux2!(u1, u2, u3, u4, u5, u6, u7, u9, u10, u11, u12, u13, u14, u15);

macro_rules! impl_backing_storage_for_primitives {
    ($($ty:ident),*) => {
        $(
            impl BackingStorage for $ty {
                const MASK: u64 = if $ty::BITS < 64 { (1 << $ty::BITS) - 1 } else { !0 };
                const BITS: usize = $ty::BITS as usize;
                #[inline]
                fn to_u64(self) -> u64 { self as u64 }
                #[inline]
                fn from_u64(value: u64) -> Self { value as _ }
                const ZERO: Self = 0;
            }
        )*
    };
}

impl_backing_storage_for_primitives!(u8, u16, u32, u64);

impl<T: BackingStorage, const N: usize> BackingStorage for [T; N] {
    const BITS: usize = T::BITS * N;
    const MASK: u64 = {
        let mut mask = 0;
        let mut i = 0;
        while i < N {
            mask |= T::MASK << (i * T::BITS);
            i += 1;
        }
        mask
    };

    #[inline]
    fn to_u64(self) -> u64 {
        let mut out = 0;
        for (i, x) in self.iter().enumerate() {
            out |= x.to_u64() << (i * T::BITS);
        }
        out
    }

    #[inline]
    fn from_u64(mut value: u64) -> Self {
        let mut out = [T::ZERO; N];
        for i in 0..N {
            out[i] = T::from_u64(value & T::MASK);
            value >>= T::BITS;
        }
        out
    }

    const ZERO: Self = [T::ZERO; N];
}

unsafe impl ZoruaField for u8 {}

unsafe impl ZoruaField for () {}

unsafe impl<const N: usize, T: ZoruaField> ZoruaField for [T; N] {}

macro_rules! impl_zorua_field_for_tuple {
    ($($T:ident),*) => {
        unsafe impl<$($T: ZoruaField),*> ZoruaField for ($($T,)*) {}
    };
}

impl_zorua_field_for_tuple!(T, U);
impl_zorua_field_for_tuple!(T, U, V);
impl_zorua_field_for_tuple!(T, U, V, W);
impl_zorua_field_for_tuple!(T, U, V, W, X);
impl_zorua_field_for_tuple!(T, U, V, W, X, Y);
impl_zorua_field_for_tuple!(T, U, V, W, X, Y, Z);
