//! Type aliases for endian-aware integer types
//!
//! This module provides convenient type aliases for endian-aware integer types.
//! Each type has both big-endian (`_be`) and little-endian (`_le`) variants.
#![allow(non_camel_case_types)]

use super::{Big, I16, I32, I64, Little, U16, U32, U64};

// Unsigned big-endian aliases
pub type u16_be = U16<Big>;
pub type u32_be = U32<Big>;
pub type u64_be = U64<Big>;

// Unsigned little-endian aliases
pub type u16_le = U16<Little>;
pub type u32_le = U32<Little>;
pub type u64_le = U64<Little>;

// Signed big-endian aliases
pub type i16_be = I16<Big>;
pub type i32_be = I32<Big>;
pub type i64_be = I64<Big>;

// Signed little-endian aliases
pub type i16_le = I16<Little>;
pub type i32_le = I32<Little>;
pub type i64_le = I64<Little>;
