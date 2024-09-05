use elain::{Align, Alignment};
use std::ops::{Deref, DerefMut};

/// A smart pointer for a length `N` array that guarantees an alignment of `ALIGN`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct AlignedBytes<const ALIGN: usize, const N: usize>
where
    Align<ALIGN>: Alignment,
{
    _alignment: [Align<ALIGN>; 0],
    value: [u8; N],
}

impl<const ALIGN: usize, const N: usize> AlignedBytes<ALIGN, N>
where
    Align<ALIGN>: Alignment,
{
    /// For larger allocations better suited to the heap, use [AlignedBytes::new_boxed].
    pub const fn new() -> AlignedBytes<ALIGN, N> {
        AlignedBytes {
            _alignment: [],
            value: [0; N],
        }
    }

    pub const fn from_bytes(bytes: [u8; N]) -> AlignedBytes<ALIGN, N> {
        AlignedBytes {
            _alignment: [],
            value: bytes,
        }
    }

    /// Use this for large allocations better suited to the heap.
    ///
    /// This fn also deals with [the boxed array issue in Rust](https://github.com/rust-lang/rust/issues/53827).
    pub fn new_boxed() -> Box<AlignedBytes<ALIGN, N>> {
        let layout = std::alloc::Layout::new::<AlignedBytes<ALIGN, N>>();
        unsafe {
            let ptr = std::alloc::alloc(layout) as *mut AlignedBytes<ALIGN, N>;
            Box::from_raw(ptr)
        }
    }
}

impl<const ALIGN: usize, const N: usize> Default for AlignedBytes<ALIGN, N>
where
    Align<ALIGN>: Alignment,
{
    fn default() -> Self {
        Self::new()
    }
}

// -------------------------
// Deref Impls
// -------------------------
impl<const ALIGN: usize, const N: usize> Deref for AlignedBytes<ALIGN, N>
where
    Align<ALIGN>: Alignment,
{
    type Target = [u8; N];

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<const ALIGN: usize, const N: usize> DerefMut for AlignedBytes<ALIGN, N>
where
    Align<ALIGN>: Alignment,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}
