//! Endian-aware integer types
//!
//! This module provides integer types that are aware of their byte order (endianness).
//! Each type is parameterized by an endianness marker type (either `Big` or `Little`).
//!
//! # Examples
//!
//! ```
//! use zorua::prelude::{U16, U32, U64, Big, Little};
//!
//! // Create a big-endian u32
//! let be_value: U32<Big> = U32::from(0x12345678u32);
//!
//! // Create a little-endian u64 from a smaller type
//! let le_value: U64<Little> = U64::from(42u16);
//!
//! // Convert back to native representation
//! let native_value = be_value.to_native();
//!
//! // Create from byte array using num_traits
//! use num_traits::FromBytes;
//! let value: U16<Big> = FromBytes::from_be_bytes(&[0x12, 0x34]);
//! ```

mod conversions;

use core::{fmt::Debug, marker::PhantomData};

// Marker types for endianness
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Big;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Little;

pub trait Endian: Copy + Default + Eq + Debug {
    /// True for big-endian, false for little-endian
    const IS_BIG_ENDIAN: bool;
}

impl Endian for Big {
    const IS_BIG_ENDIAN: bool = true;
}

impl Endian for Little {
    const IS_BIG_ENDIAN: bool = false;
}

// Macro to generate endian-aware integer types
#[macro_export]
macro_rules! define_endian_int {
    ($name:ident, $primitive:ty) => {
        /// An endian-aware wrapper around a primitive integer type
        #[repr(transparent)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name<E: Endian> {
            bytes: [u8; std::mem::size_of::<$primitive>()],
            _endian: PhantomData<E>,
        }

        impl<E: Endian> $name<E> {
            pub const fn from_native(value: $primitive) -> Self {
                let bytes = if E::IS_BIG_ENDIAN {
                    value.to_be_bytes()
                } else {
                    value.to_le_bytes()
                };
                Self {
                    bytes,
                    _endian: PhantomData,
                }
            }

            pub const fn to_native(&self) -> $primitive {
                if E::IS_BIG_ENDIAN {
                    <$primitive>::from_be_bytes(self.bytes)
                } else {
                    <$primitive>::from_le_bytes(self.bytes)
                }
            }
        }

        // From implementation for the primitive type
        impl<E: Endian> From<$primitive> for $name<E> {
            fn from(value: $primitive) -> Self {
                Self::from_native(value)
            }
        }
    };
}

// Define all the unsigned types
define_endian_int!(U8, u8);
define_endian_int!(U16, u16);
define_endian_int!(U32, u32);
define_endian_int!(U64, u64);

// Define all the signed types
define_endian_int!(I8, i8);
define_endian_int!(I16, i16);
define_endian_int!(I32, i32);
define_endian_int!(I64, i64);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_u32_endian() {
        // Big-endian roundtrip
        let big_endian_value: U32<Big> = U32::from(0x12345678u32);
        assert_eq!(big_endian_value.to_native(), 0x12345678);

        // Little-endian roundtrip
        let little_endian_value: U32<Little> = U32::from(0x12345678u32);
        assert_eq!(little_endian_value.to_native(), 0x12345678);
    }

    #[test]
    fn test_endian_behavior() {
        // Test that big and little-endian actually store bytes differently
        let big_val: U16<Big> = U16::from(0x1234u16);
        let little_val: U16<Little> = U16::from(0x1234u16);

        // Both should convert back to the same native value
        assert_eq!(big_val.to_native(), 0x1234);
        assert_eq!(little_val.to_native(), 0x1234);

        // But their byte representations should be different - let's transmute the U16s to [u8; 2] and compare
        let big_bytes: [u8; 2] = unsafe { std::mem::transmute(big_val) };
        let little_bytes: [u8; 2] = unsafe { std::mem::transmute(little_val) };
        assert_ne!(big_bytes, little_bytes);
    }

    #[test]
    fn test_basic_integer_types() {
        // Test U8
        let u8_val: U8<Big> = U8::from(255u8);
        assert_eq!(u8_val.to_native(), 255);

        // Test U16
        let u16_val: U16<Little> = U16::from(65535u16);
        assert_eq!(u16_val.to_native(), 65535);

        // Test U64
        let u64_val: U64<Big> = U64::from(0xFFFFFFFFFFFFFFFFu64);
        assert_eq!(u64_val.to_native(), 0xFFFFFFFFFFFFFFFF);

        // Test signed types
        let i8_val: I8<Little> = I8::from(-128i8);
        assert_eq!(i8_val.to_native(), -128);

        let i16_val: I16<Big> = I16::from(-32768i16);
        assert_eq!(i16_val.to_native(), -32768);

        let i32_val: I32<Little> = I32::from(-2147483648i32);
        assert_eq!(i32_val.to_native(), -2147483648);

        let i64_val: I64<Big> = I64::from(-9223372036854775808i64);
        assert_eq!(i64_val.to_native(), -9223372036854775808);
    }
}
