//! Conversion implementations for endian-aware types to/from primitive types

use super::*;

use std::convert::TryFrom;

// Macro to implement From for smaller unsigned types
macro_rules! impl_from_smaller_unsigned {
    ($from:ty, $to:ident, $to_primitive:ty) => {
        impl<E: Endian> From<$from> for $to<E> {
            fn from(value: $from) -> Self {
                Self::from(value as $to_primitive)
            }
        }
    };
}

// Macro to implement From for smaller signed types
macro_rules! impl_from_smaller_signed {
    ($from:ty, $to:ident, $to_primitive:ty) => {
        impl<E: Endian> From<$from> for $to<E> {
            fn from(value: $from) -> Self {
                Self::from(value as $to_primitive)
            }
        }
    };
}

// Macro to implement TryFrom for potentially overflowing conversions
macro_rules! impl_try_from {
    ($from:ty, $to:ident, $to_primitive:ty) => {
        impl<E: Endian> TryFrom<$from> for $to<E> {
            type Error = std::num::TryFromIntError;

            fn try_from(value: $from) -> Result<Self, Self::Error> {
                <$to_primitive>::try_from(value).map(Self::from)
            }
        }
    };
}

// Macro to implement From for endian types to primitives
macro_rules! impl_from_endian_to_primitive {
    ($endian_type:ident, $primitive:ty) => {
        impl<E: Endian> From<$endian_type<E>> for $primitive {
            fn from(value: $endian_type<E>) -> Self {
                value.value()
            }
        }
    };
}

// Macro to implement TryFrom for endian types to smaller primitives
macro_rules! impl_try_from_endian_to_smaller {
    ($endian_type:ident, $from_primitive:ty, $to_primitive:ty) => {
        impl<E: Endian> TryFrom<$endian_type<E>> for $to_primitive {
            type Error = std::num::TryFromIntError;

            fn try_from(value: $endian_type<E>) -> Result<Self, Self::Error> {
                <$to_primitive>::try_from(value.value())
            }
        }
    };
}

// From impls for primitives to endian types

// Unsigned types - From smaller to larger
impl_from_smaller_unsigned!(u8, U16, u16);
impl_from_smaller_unsigned!(u8, U32, u32);
impl_from_smaller_unsigned!(u16, U32, u32);
impl_from_smaller_unsigned!(u8, U64, u64);
impl_from_smaller_unsigned!(u16, U64, u64);
impl_from_smaller_unsigned!(u32, U64, u64);

// Signed types - From smaller signed to larger signed
impl_from_smaller_signed!(i8, I16, i16);
impl_from_smaller_signed!(i8, I32, i32);
impl_from_smaller_signed!(i16, I32, i32);
impl_from_smaller_signed!(i8, I64, i64);
impl_from_smaller_signed!(i16, I64, i64);
impl_from_smaller_signed!(i32, I64, i64);

// Safe unsigned to signed conversions (always fit)
impl_from_smaller_unsigned!(u8, I16, i16);
impl_from_smaller_unsigned!(u8, I32, i32);
impl_from_smaller_unsigned!(u16, I32, i32);
impl_from_smaller_unsigned!(u8, I64, i64);
impl_from_smaller_unsigned!(u16, I64, i64);
impl_from_smaller_unsigned!(u32, I64, i64);

// From impls for endian types to their native primitives
impl_from_endian_to_primitive!(U8, u8);
impl_from_endian_to_primitive!(U16, u16);
impl_from_endian_to_primitive!(U32, u32);
impl_from_endian_to_primitive!(U64, u64);
impl_from_endian_to_primitive!(I8, i8);
impl_from_endian_to_primitive!(I16, i16);
impl_from_endian_to_primitive!(I32, i32);
impl_from_endian_to_primitive!(I64, i64);

// TryFrom impls for endian types to smaller primitives
// Unsigned endian to smaller unsigned primitives
impl_try_from_endian_to_smaller!(U16, u16, u8);
impl_try_from_endian_to_smaller!(U32, u32, u8);
impl_try_from_endian_to_smaller!(U32, u32, u16);
impl_try_from_endian_to_smaller!(U64, u64, u8);
impl_try_from_endian_to_smaller!(U64, u64, u16);
impl_try_from_endian_to_smaller!(U64, u64, u32);

// Signed endian to smaller signed primitives
impl_try_from_endian_to_smaller!(I16, i16, i8);
impl_try_from_endian_to_smaller!(I32, i32, i8);
impl_try_from_endian_to_smaller!(I32, i32, i16);
impl_try_from_endian_to_smaller!(I64, i64, i8);
impl_try_from_endian_to_smaller!(I64, i64, i16);
impl_try_from_endian_to_smaller!(I64, i64, i32);

// Cross-type conversions (signed to unsigned, unsigned to signed)
// Signed endian types to unsigned primitives
impl_try_from_endian_to_smaller!(I8, i8, u8);
impl_try_from_endian_to_smaller!(I16, i16, u8);
impl_try_from_endian_to_smaller!(I16, i16, u16);
impl_try_from_endian_to_smaller!(I32, i32, u8);
impl_try_from_endian_to_smaller!(I32, i32, u16);
impl_try_from_endian_to_smaller!(I32, i32, u32);
impl_try_from_endian_to_smaller!(I64, i64, u8);
impl_try_from_endian_to_smaller!(I64, i64, u16);
impl_try_from_endian_to_smaller!(I64, i64, u32);
impl_try_from_endian_to_smaller!(I64, i64, u64);

// Unsigned endian types to signed primitives
impl_try_from_endian_to_smaller!(U8, u8, i8);
impl_try_from_endian_to_smaller!(U16, u16, i8);
impl_try_from_endian_to_smaller!(U16, u16, i16);
impl_try_from_endian_to_smaller!(U32, u32, i8);
impl_try_from_endian_to_smaller!(U32, u32, i16);
impl_try_from_endian_to_smaller!(U32, u32, i32);
impl_try_from_endian_to_smaller!(U64, u64, i8);
impl_try_from_endian_to_smaller!(U64, u64, i16);
impl_try_from_endian_to_smaller!(U64, u64, i32);
impl_try_from_endian_to_smaller!(U64, u64, i64);

// TryFrom impls for primitives to endian types
// Unsigned to smaller unsigned endian types
impl_try_from!(u16, U8, u8);
impl_try_from!(u32, U8, u8);
impl_try_from!(u32, U16, u16);
impl_try_from!(u64, U8, u8);
impl_try_from!(u64, U16, u16);
impl_try_from!(u64, U32, u32);

// Signed to smaller signed endian types
impl_try_from!(i16, I8, i8);
impl_try_from!(i32, I8, i8);
impl_try_from!(i32, I16, i16);
impl_try_from!(i64, I8, i8);
impl_try_from!(i64, I16, i16);
impl_try_from!(i64, I32, i32);

// Signed to unsigned endian types
impl_try_from!(i8, U8, u8);
impl_try_from!(i8, U16, u16);
impl_try_from!(i8, U32, u32);
impl_try_from!(i8, U64, u64);
impl_try_from!(i16, U8, u8);
impl_try_from!(i16, U16, u16);
impl_try_from!(i16, U32, u32);
impl_try_from!(i16, U64, u64);
impl_try_from!(i32, U8, u8);
impl_try_from!(i32, U16, u16);
impl_try_from!(i32, U32, u32);
impl_try_from!(i32, U64, u64);
impl_try_from!(i64, U8, u8);
impl_try_from!(i64, U16, u16);
impl_try_from!(i64, U32, u32);
impl_try_from!(i64, U64, u64);

// Unsigned to signed endian types
impl_try_from!(u8, I8, i8);
impl_try_from!(u16, I8, i8);
impl_try_from!(u16, I16, i16);
impl_try_from!(u32, I8, i8);
impl_try_from!(u32, I16, i16);
impl_try_from!(u32, I32, i32);
impl_try_from!(u64, I8, i8);
impl_try_from!(u64, I16, i16);
impl_try_from!(u64, I32, i32);
impl_try_from!(u64, I64, i64);

#[cfg(test)]
mod tests {
    use super::*;

    use std::convert::TryInto;

    #[test]
    fn test_basic_conversions() {
        // Round-trip: u8 -> U32 -> u32
        let original = 255u8;
        let endian_value: U32<Big> = original.into();
        let back_to_primitive: u32 = endian_value.into();
        assert_eq!(back_to_primitive, original as u32);

        // Round-trip: u16 -> U64 -> u64
        let original = 65535u16;
        let endian_value: U64<Little> = original.into();
        let back_to_primitive: u64 = endian_value.into();
        assert_eq!(back_to_primitive, original as u64);

        // Round-trip: i8 -> I32 -> i32
        let original = -128i8;
        let endian_value: I32<Little> = original.into();
        let back_to_primitive: i32 = endian_value.into();
        assert_eq!(back_to_primitive, original as i32);

        // Round-trip: i16 -> I64 -> i64
        let original = -32768i16;
        let endian_value: I64<Big> = original.into();
        let back_to_primitive: i64 = endian_value.into();
        assert_eq!(back_to_primitive, original as i64);
    }

    #[test]
    fn test_try_conversions_successful() {
        // Successful downsizing: u32 -> U16 -> u16
        let original = 1000u32;
        let endian_result: Result<U16<Big>, _> = original.try_into();
        assert!(endian_result.is_ok());
        let endian_value = endian_result.unwrap();
        let back_to_primitive: u16 = endian_value.into();
        assert_eq!(back_to_primitive, original as u16);

        // Successful cross-type: i16 -> U16 -> u16 (positive value)
        let original = 1000i16;
        let endian_result: Result<U16<Big>, _> = original.try_into();
        assert!(endian_result.is_ok());
        let endian_value = endian_result.unwrap();
        let back_to_primitive: u16 = endian_value.into();
        assert_eq!(back_to_primitive, original as u16);

        // Successful endian to smaller: U32 -> u16
        let original = 42u32;
        let endian_value: U32<Little> = original.into();
        let smaller_result: Result<u16, _> = endian_value.try_into();
        assert!(smaller_result.is_ok());
        assert_eq!(smaller_result.unwrap(), original as u16);
    }

    #[test]
    fn test_try_conversions_overflow() {
        // Overflow: u32 -> U16 (too large)
        let large_value = 70000u32;
        let result: Result<U16<Big>, _> = large_value.try_into();
        assert!(result.is_err());

        // Overflow: U32 -> u16 (too large)
        let large_value = 70000u32;
        let endian_value: U32<Big> = large_value.into();
        let result: Result<u16, _> = endian_value.try_into();
        assert!(result.is_err());

        // Negative to unsigned: i16 -> U16 (negative value)
        let negative_value = -1000i16;
        let result: Result<U16<Big>, _> = negative_value.try_into();
        assert!(result.is_err());

        // Large unsigned to signed: U16 -> i16 (too large)
        let large_value = 40000u16;
        let endian_value: U16<Little> = large_value.into();
        let result: Result<i16, _> = endian_value.try_into();
        assert!(result.is_err());
    }

    #[test]
    fn test_cross_type_conversions() {
        // Positive signed to unsigned: i32 -> U32 -> u32
        let original = 1000i32;
        let endian_result: Result<U32<Big>, _> = original.try_into();
        assert!(endian_result.is_ok());
        let endian_value = endian_result.unwrap();
        let back_to_primitive: u32 = endian_value.into();
        assert_eq!(back_to_primitive, original as u32);

        // Endian cross-type: I16 -> u16 (positive)
        let original = 1000i16;
        let endian_value: I16<Big> = original.into();
        let cross_result: Result<u16, _> = endian_value.try_into();
        assert!(cross_result.is_ok());
        assert_eq!(cross_result.unwrap(), original as u16);

        // Endian cross-type: U16 -> i16 (small value)
        let original = 1000u16;
        let endian_value: U16<Little> = original.into();
        let cross_result: Result<i16, _> = endian_value.try_into();
        assert!(cross_result.is_ok());
        assert_eq!(cross_result.unwrap(), original as i16);
    }
}
