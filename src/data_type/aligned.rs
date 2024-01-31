use core::{
    cmp::Ordering,
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    ops,
};

/// 2-byte alignment
#[derive(Clone, Copy)]
#[repr(align(1))]
pub struct A1;

/// 2-byte alignment
#[derive(Clone, Copy)]
#[repr(align(2))]
pub struct A2;

/// 4-byte alignment
#[derive(Clone, Copy)]
#[repr(align(4))]
pub struct A4;

/// 8-byte alignment
#[derive(Clone, Copy)]
#[repr(align(8))]
pub struct A8;

/// 16-byte alignment
#[derive(Clone, Copy)]
#[repr(align(16))]
pub struct A16;

/// 32-byte alignment
#[derive(Clone, Copy)]
#[repr(align(32))]
pub struct A32;

/// 64-byte alignment
#[derive(Clone, Copy)]
#[repr(align(64))]
pub struct A64;

/// 128-byte alignment
#[derive(Clone, Copy)]
#[repr(align(128))]
pub struct A128;

pub trait Alignment: Copy {}
impl Alignment for A1 {}
impl Alignment for A2 {}
impl Alignment for A4 {}
impl Alignment for A8 {}
impl Alignment for A16 {}
impl Alignment for A32 {}
impl Alignment for A64 {}
impl Alignment for A128 {}

/// A newtype with alignment of at least `A` bytes
#[repr(C)]
pub struct Aligned<A: Alignment, T: ?Sized> {
    _alignment: [A; 0],
    value: T,
}

/// Changes the alignment of `value` to be at least `A` bytes
#[allow(non_snake_case)]
pub const fn Aligned<A: Alignment, T>(value: T) -> Aligned<A, T> {
    Aligned {
        _alignment: [],
        value,
    }
}

impl<A: Alignment, T: ?Sized> ops::Deref for Aligned<A, T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

impl<A: Alignment, T: ?Sized> ops::DerefMut for Aligned<A, T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<A: Alignment, T> ops::Index<ops::RangeTo<usize>> for Aligned<A, [T]> {
    type Output = Aligned<A, [T]>;

    fn index(&self, range: ops::RangeTo<usize>) -> &Aligned<A, [T]> {
        unsafe { &*(&self.value[range] as *const [T] as *const Aligned<A, [T]>) }
    }
}

impl<A: Alignment, T: Clone> Clone for Aligned<A, T> {
    fn clone(&self) -> Self {
        Self {
            _alignment: [],
            value: self.value.clone(),
        }
    }
}

impl<A: Alignment, T: Copy> Copy for Aligned<A, T> {}

impl<A: Alignment, T: Default> Default for Aligned<A, T> {
    fn default() -> Self {
        Self {
            _alignment: [],
            value: Default::default(),
        }
    }
}

impl<A: Alignment, T: Debug> Debug for Aligned<A, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.value.fmt(f)
    }
}

impl<A: Alignment, T: Display> Display for Aligned<A, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.value.fmt(f)
    }
}

impl<A: Alignment, T: PartialEq> PartialEq for Aligned<A, T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<A: Alignment, T> Eq for Aligned<A, T> where T: Eq {}

impl<A: Alignment, T: Hash> Hash for Aligned<A, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl<A: Alignment, T: Ord> Ord for Aligned<A, T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.value.cmp(&other.value)
    }
}

impl<A: Alignment, T: PartialOrd> PartialOrd for Aligned<A, T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value.partial_cmp(&other.value)
    }
}
