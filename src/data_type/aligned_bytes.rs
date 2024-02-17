pub use alignment::*;
use std::ops::{Deref, DerefMut};

/// A smart pointer for a length `N` array that guarantees its [Alignment].
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct AlignedBytes<A: Alignment, const N: usize> {
    _alignment: [A; 0],
    value: [u8; N],
}

impl<A: Alignment, const N: usize> AlignedBytes<A, N> {
    pub const fn new() -> AlignedBytes<A, N> {
        AlignedBytes {
            _alignment: [],
            value: [0; N],
        }
    }

    pub const fn from_bytes(bytes: [u8; N]) -> AlignedBytes<A, N> {
        AlignedBytes {
            _alignment: [],
            value: bytes,
        }
    }
}

// -------------------------
// Deref Impls
// -------------------------
impl<A: Alignment, const N: usize> Deref for AlignedBytes<A, N> {
    type Target = [u8; N];

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<A: Alignment, const N: usize> DerefMut for AlignedBytes<A, N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

mod sealed {
    pub trait Sealed {}
}

mod alignment {
    use super::sealed::Sealed;

    /// 2-byte alignment
    #[derive(Clone, Copy, Debug)]
    #[repr(align(1))]
    pub struct A1;

    /// 2-byte alignment
    #[derive(Clone, Copy, Debug)]
    #[repr(align(2))]
    pub struct A2;

    /// 4-byte alignment
    #[derive(Clone, Copy, Debug)]
    #[repr(align(4))]
    pub struct A4;

    /// 8-byte alignment
    #[derive(Clone, Copy, Debug)]
    #[repr(align(8))]
    pub struct A8;

    /// 16-byte alignment
    #[derive(Clone, Copy, Debug)]
    #[repr(align(16))]
    pub struct A16;

    /// 32-byte alignment
    #[derive(Clone, Copy, Debug)]
    #[repr(align(32))]
    pub struct A32;

    /// 64-byte alignment
    #[derive(Clone, Copy, Debug)]
    #[repr(align(64))]
    pub struct A64;

    /// 128-byte alignment
    #[derive(Clone, Copy)]
    #[repr(align(128))]
    pub struct A128;

    /// Represents the alignment of a type, which is some power of 2.
    pub trait Alignment: Copy + Sealed {} //Sealed makes this unimplementable
    impl Alignment for A1 {}
    impl Alignment for A2 {}
    impl Alignment for A4 {}
    impl Alignment for A8 {}
    impl Alignment for A16 {}
    impl Alignment for A32 {}
    impl Alignment for A64 {}
    impl Alignment for A128 {}

    impl Sealed for A1 {}
    impl Sealed for A2 {}
    impl Sealed for A4 {}
    impl Sealed for A8 {}
    impl Sealed for A16 {}
    impl Sealed for A32 {}
    impl Sealed for A64 {}
    impl Sealed for A128 {}
}
