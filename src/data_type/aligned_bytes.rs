use core::ops::{Deref, DerefMut};

use elain::{Align, Alignment};

/// A smart pointer for a length `N` array that guarantees an alignment of `ALIGN`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct AlignedBytes<const ALIGN: usize, const N: usize>
where
    Align<ALIGN>: Alignment,
{
    _alignment: [Align<ALIGN>; 0],
    value: [u8; N],
}

// Manual ZoruaField impl with compile-time padding check
unsafe impl<const ALIGN: usize, const N: usize> crate::traits::ZoruaField for AlignedBytes<ALIGN, N> where
    Align<ALIGN>: Alignment
{
}

// Compile-time check that alignment divides size (no padding issues)
const fn _check_aligned_bytes<const ALIGN: usize, const N: usize>()
where
    Align<ALIGN>: Alignment,
{
    assert!(
        N.is_multiple_of(ALIGN) || N == 0,
        "AlignedBytes size must be divisible by alignment"
    );
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

    /// Returns an iterator over fixed-size chunks of this buffer.
    ///
    /// Compile-time checks that N is evenly divisible by CHUNK_SIZE.
    pub fn array_chunks<const CHUNK_SIZE: usize>(&self) -> impl Iterator<Item = &[u8; CHUNK_SIZE]> {
        const {
            assert!(
                N.is_multiple_of(CHUNK_SIZE),
                "buffer size must be divisible by chunk size"
            )
        };
        self.value
            .chunks_exact(CHUNK_SIZE)
            .map(|s| s.try_into().unwrap())
    }

    /// Returns a mutable iterator over fixed-size chunks of this buffer.
    ///
    /// Compile-time checks that N is evenly divisible by CHUNK_SIZE.
    pub fn array_chunks_mut<const CHUNK_SIZE: usize>(
        &mut self,
    ) -> impl Iterator<Item = &mut [u8; CHUNK_SIZE]> {
        const {
            assert!(
                N.is_multiple_of(CHUNK_SIZE),
                "buffer size must be divisible by chunk size"
            )
        };
        self.value
            .chunks_exact_mut(CHUNK_SIZE)
            .map(|s| s.try_into().unwrap())
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
    {
        const {
            assert!(
                ALIGN >= NEW_ALIGN,
                "new alignment must be <= current alignment"
            )
        };
        AlignedBytes {
            _alignment: [],
            value: self.value,
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
