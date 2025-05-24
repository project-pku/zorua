use core::{
    mem,
    ops::{Deref, DerefMut},
};
use elain::{Align, Alignment};

use crate::traits::ZoruaField;

pub type AlignOf<T> = Align<{ mem::align_of::<T>() }>;

/// A smart pointer for a length `N` array that guarantees an alignment of `ALIGN`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AlignedBytes<const ALIGN: usize, const N: usize>
where
    Align<ALIGN>: Alignment,
{
    _alignment: [Align<ALIGN>; 0],
    value: [u8; N],
}

unsafe impl<const ALIGN: usize, const N: usize> ZoruaField for AlignedBytes<ALIGN, N>
where
    Align<ALIGN>: Alignment,
    [(); (N % ALIGN == 0) as usize - 1]: Sized,
{
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

    pub fn from_bytes(bytes: &[u8; N]) -> AlignedBytes<ALIGN, N> {
        AlignedBytes {
            _alignment: [],
            value: *bytes,
        }
    }

    /// Use this for large allocations better suited to the heap.
    ///
    /// This fn also deals with [the boxed array issue in Rust](https://github.com/rust-lang/rust/issues/53827).
    #[cfg(feature = "std")]
    pub fn new_boxed() -> Box<AlignedBytes<ALIGN, N>> {
        let layout = core::alloc::Layout::new::<AlignedBytes<ALIGN, N>>();
        unsafe {
            let ptr = std::alloc::alloc(layout) as *mut AlignedBytes<ALIGN, N>;
            Box::from_raw(ptr)
        }
    }

    pub fn chunk<const CHUNK_SIZE: usize>(self) -> [AlignedBytes<ALIGN, CHUNK_SIZE>; N / CHUNK_SIZE]
    where
        [(); (N % CHUNK_SIZE == 0) as usize - 1]: Sized,
    {
        unsafe { crate::unconditional_transmute(self) }
    }

    pub fn chunk_ref<const CHUNK_SIZE: usize>(
        &self,
    ) -> &[AlignedBytes<ALIGN, CHUNK_SIZE>; N / CHUNK_SIZE]
    where
        [(); (N % CHUNK_SIZE == 0) as usize - 1]: Sized,
    {
        unsafe { mem::transmute(self) }
    }

    pub fn chunk_mut<const CHUNK_SIZE: usize>(
        &mut self,
    ) -> &mut [AlignedBytes<ALIGN, CHUNK_SIZE>; N / CHUNK_SIZE]
    where
        [(); (N % CHUNK_SIZE == 0) as usize - 1]: Sized,
    {
        unsafe { mem::transmute(self) }
    }

    /// Transmutes an [`AlignedBytes`] type into another `AlignedBytes`
    /// type with a potentially weaker (i.e. lower) alignment.
    ///
    /// Usage:
    /// ```
    /// use zorua::prelude::*;
    ///
    /// let x = AlignedBytes::<4, 20>::new();
    /// let y: AlignedBytes<2, 20> = x.weaken_alignment::<2>();
    /// ```
    ///
    /// Note that the fn will only compile if:
    /// - The desired alignment is a power of 2.
    /// - The new alignment is equal or weaker (i.e. lower) than the old alignment.
    pub fn weaken_alignment<const NEW_ALIGN: usize>(self) -> AlignedBytes<NEW_ALIGN, N>
    where
        Align<NEW_ALIGN>: Alignment,
        [(); (ALIGN >= NEW_ALIGN) as usize - 1]:,
    {
        unsafe { crate::unconditional_transmute(self) }
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
