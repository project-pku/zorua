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

// From impls for U16
impl_from_smaller_unsigned!(u8, U16, u16);

// From impls for U32
impl_from_smaller_unsigned!(u8, U32, u32);
impl_from_smaller_unsigned!(u16, U32, u32);

// From impls for U64
impl_from_smaller_unsigned!(u8, U64, u64);
impl_from_smaller_unsigned!(u16, U64, u64);
impl_from_smaller_unsigned!(u32, U64, u64);

// From impls for I16
impl_from_smaller_signed!(i8, I16, i16);

// From impls for I32
impl_from_smaller_signed!(i8, I32, i32);
impl_from_smaller_signed!(i16, I32, i32);

// From impls for I64
impl_from_smaller_signed!(i8, I64, i64);
impl_from_smaller_signed!(i16, I64, i64);
impl_from_smaller_signed!(i32, I64, i64);

// TryFrom impls for unsigned to smaller unsigned
impl_try_from!(u16, U8, u8);
impl_try_from!(u32, U8, u8);
impl_try_from!(u32, U16, u16);
impl_try_from!(u64, U8, u8);
impl_try_from!(u64, U16, u16);
impl_try_from!(u64, U32, u32);

// TryFrom impls for signed to smaller signed
impl_try_from!(i16, I8, i8);
impl_try_from!(i32, I8, i8);
impl_try_from!(i32, I16, i16);
impl_try_from!(i64, I8, i8);
impl_try_from!(i64, I16, i16);
impl_try_from!(i64, I32, i32);

// TryFrom impls for signed to unsigned
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

// TryFrom impls for unsigned to signed
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
    fn test_from_smaller_types() {
        // From u8 to U32
        let value: U32<Big> = U32::from(255u8);
        assert_eq!(value.to_native(), 255);

        // From u16 to U64
        let value: U64<Little> = U64::from(65535u16);
        assert_eq!(value.to_native(), 65535);
    }

    #[test]
    fn test_try_from_overflow() {
        // Try to convert u32 to U16 (might overflow)
        let result: Result<U16<Big>, _> = 70000u32.try_into();
        assert!(result.is_err());

        // Successful conversion
        let result: Result<U16<Big>, _> = 1000u32.try_into();
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_native(), 1000);
    }

    #[test]
    fn test_signed_conversions() {
        // From i8 to I32
        let value: I32<Little> = I32::from(-128i8);
        assert_eq!(value.to_native(), -128);

        // From i16 to I64
        let value: I64<Big> = I64::from(-32768i16);
        assert_eq!(value.to_native(), -32768);
    }

    #[test]
    fn test_signed_to_unsigned_try_from() {
        // Try to convert negative i8 to U8 (should fail)
        let result: Result<U8<Big>, _> = (-1i8).try_into();
        assert!(result.is_err());

        // Successful conversion of positive i8 to U8
        let result: Result<U8<Big>, _> = 127i8.try_into();
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_native(), 127);

        // Try to convert large u32 to I16 (should fail)
        let result: Result<I16<Little>, _> = 40000u32.try_into();
        assert!(result.is_err());

        // Successful conversion
        let result: Result<I16<Little>, _> = 1000u32.try_into();
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_native(), 1000);
    }
}
