use std::{
    fmt::Display,
    ops::{
        Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Div,
        DivAssign, Mul, MulAssign, Not, Rem, RemAssign, Shl, Shr, Sub, SubAssign,
    },
    str::FromStr,
};

use num_traits::{
    Bounded, CheckedAdd, CheckedDiv, CheckedMul, CheckedSub, ConstOne, ConstZero, FromBytes,
    FromPrimitive, Num, NumCast, One, PrimInt, Saturating, ToBytes, ToPrimitive, WrappingAdd,
    WrappingMul, WrappingSub, Zero,
};

use super::*;

// NumOps is automatically implemented when Add, Sub, Mul, Div, Rem are implemented
// NumRef is automatically implemented when NumOps is implemented for references
// RefNum is automatically implemented when NumOps is implemented for &T

// NumAssignOps is automatically implemented when AddAssign, SubAssign, MulAssign, DivAssign, RemAssign are implemented
// NumAssign is automatically implemented when Num + NumAssignOps
// NumAssignRef is automatically implemented when NumAssign is implemented with reference operations

// Macro to implement Num trait
macro_rules! impl_num {
    ($($t:ident => $primitive:ty),*) => {
        $(
            impl<E: Endian> Num for $t<E> {
                type FromStrRadixErr = <$primitive as Num>::FromStrRadixErr;

                fn from_str_radix(str: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
                    <$primitive>::from_str_radix(str, radix).map(<Self as From<$primitive>>::from)
                }
            }
        )*
    };
}

impl_num!(
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);

// Macro to implement shift operations
macro_rules! impl_shift_ops {
    ($($t:ident => $primitive:ty),*) => {
        $(
            impl<E: Endian> Shl<usize> for $t<E> {
                type Output = Self;

                fn shl(self, rhs: usize) -> Self::Output {
                    <Self as From<$primitive>>::from(self.to_native() << rhs)
                }
            }

            impl<E: Endian> Shr<usize> for $t<E> {
                type Output = Self;

                fn shr(self, rhs: usize) -> Self::Output {
                    <Self as From<$primitive>>::from(self.to_native() >> rhs)
                }
            }
        )*
    };
}

impl_shift_ops!(
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);

// Macro to implement bitwise operations
macro_rules! impl_bitwise_ops {
    ($($t:ident => $primitive:ty),*) => {
        $(
            impl<E: Endian> BitAnd for $t<E> {
                type Output = Self;

                fn bitand(self, rhs: Self) -> Self::Output {
                    <Self as From<$primitive>>::from(self.to_native() & rhs.to_native())
                }
            }

            impl<E: Endian> BitOr for $t<E> {
                type Output = Self;

                fn bitor(self, rhs: Self) -> Self::Output {
                    <Self as From<$primitive>>::from(self.to_native() | rhs.to_native())
                }
            }

            impl<E: Endian> BitXor for $t<E> {
                type Output = Self;

                fn bitxor(self, rhs: Self) -> Self::Output {
                    <Self as From<$primitive>>::from(self.to_native() ^ rhs.to_native())
                }
            }

            impl<E: Endian> Not for $t<E> {
                type Output = Self;

                fn not(self) -> Self::Output {
                    <Self as From<$primitive>>::from(!self.to_native())
                }
            }

            impl<E: Endian> BitAndAssign for $t<E> {
                fn bitand_assign(&mut self, rhs: Self) {
                    *self = <Self as From<$primitive>>::from(self.to_native() & rhs.to_native());
                }
            }

            impl<E: Endian> BitOrAssign for $t<E> {
                fn bitor_assign(&mut self, rhs: Self) {
                    *self = <Self as From<$primitive>>::from(self.to_native() | rhs.to_native());
                }
            }

            impl<E: Endian> BitXorAssign for $t<E> {
                fn bitxor_assign(&mut self, rhs: Self) {
                    *self = <Self as From<$primitive>>::from(self.to_native() ^ rhs.to_native());
                }
            }
        )*
    };
}

impl_bitwise_ops!(
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);

// Macro to implement Bounded trait
macro_rules! impl_bounded {
    ($($t:ident => $primitive:ty),*) => {
        $(
            impl<E: Endian> Bounded for $t<E> {
                fn min_value() -> Self {
                    <Self as From<$primitive>>::from(<$primitive>::MIN)
                }

                fn max_value() -> Self {
                    <Self as From<$primitive>>::from(<$primitive>::MAX)
                }
            }
        )*
    };
}

impl_bounded!(
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);

// Macro to implement NumCast trait
macro_rules! impl_num_cast {
    ($($t:ident => $primitive:ty),*) => {
        $(
            impl<E: Endian> NumCast for $t<E> {
                fn from<T: ToPrimitive>(n: T) -> Option<Self> {
                    <$primitive as NumCast>::from(n).map(<Self as From<$primitive>>::from)
                }
            }
        )*
    };
}

impl_num_cast!(
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);

// Macro to implement PrimInt trait for unsigned types
macro_rules! impl_prim_int {
    ($($t:ident => $primitive:ty),*) => {
        $(
            impl<E: Endian> PrimInt for $t<E> {
                fn count_ones(self) -> u32 {
                    self.to_native().count_ones()
                }

                fn count_zeros(self) -> u32 {
                    self.to_native().count_zeros()
                }

                fn leading_zeros(self) -> u32 {
                    self.to_native().leading_zeros()
                }

                fn trailing_zeros(self) -> u32 {
                    self.to_native().trailing_zeros()
                }

                fn rotate_left(self, n: u32) -> Self {
                    <Self as From<$primitive>>::from(self.to_native().rotate_left(n))
                }

                fn rotate_right(self, n: u32) -> Self {
                    <Self as From<$primitive>>::from(self.to_native().rotate_right(n))
                }

                fn signed_shl(self, n: u32) -> Self {
                    <Self as From<$primitive>>::from(self.to_native() << n)
                }

                fn signed_shr(self, n: u32) -> Self {
                    <Self as From<$primitive>>::from((<$primitive as PrimInt>::signed_shr(self.to_native(), n)))
                }

                fn unsigned_shl(self, n: u32) -> Self {
                    <Self as From<$primitive>>::from(self.to_native() << n)
                }

                fn unsigned_shr(self, n: u32) -> Self {
                    <Self as From<$primitive>>::from(self.to_native() >> n)
                }

                fn swap_bytes(self) -> Self {
                    <Self as From<$primitive>>::from(self.to_native().swap_bytes())
                }

                fn from_be(x: Self) -> Self {
                    // Convert from big-endian to current endianness
                    let native = if E::IS_BIG_ENDIAN {
                        // If we're already big-endian, just return the value
                        x.to_native()
                    } else {
                        // Convert from big-endian to little-endian
                        <$primitive>::from_be(x.to_native())
                    };
                    <Self as From<$primitive>>::from(native)
                }

                fn from_le(x: Self) -> Self {
                    // Convert from little-endian to current endianness
                    let native = if E::IS_BIG_ENDIAN {
                        // Convert from little-endian to big-endian
                        <$primitive>::from_le(x.to_native())
                    } else {
                        // If we're already little-endian, just return the value
                        x.to_native()
                    };
                    <Self as From<$primitive>>::from(native)
                }

                fn to_be(self) -> Self {
                    // Convert from current endianness to big-endian
                    let native = if E::IS_BIG_ENDIAN {
                        // Already big-endian, just return native value
                        self.to_native()
                    } else {
                        // Convert to big-endian
                        self.to_native().to_be()
                    };
                    <Self as From<$primitive>>::from(native)
                }

                fn to_le(self) -> Self {
                    // Convert from current endianness to little-endian
                    let native = if E::IS_BIG_ENDIAN {
                        // Convert to little-endian
                        self.to_native().to_le()
                    } else {
                        // Already little-endian, just return native value
                        self.to_native()
                    };
                    <Self as From<$primitive>>::from(native)
                }

                fn pow(self, exp: u32) -> Self {
                    <Self as From<$primitive>>::from(self.to_native().pow(exp))
                }
            }
        )*
    };
}

impl_prim_int!(
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);

// Macro to implement ToBytes and FromBytes traits
macro_rules! impl_to_from_bytes {
    ($($t:ident => $primitive:ty),*) => {
        $(
            impl<E: Endian> ToBytes for $t<E> {
                type Bytes = [u8; std::mem::size_of::<$primitive>()];

                fn to_be_bytes(&self) -> Self::Bytes {
                    self.to_native().to_be_bytes()
                }

                fn to_le_bytes(&self) -> Self::Bytes {
                    self.to_native().to_le_bytes()
                }
            }

            impl<E: Endian> FromBytes for $t<E> {
                type Bytes = [u8; std::mem::size_of::<$primitive>()];

                fn from_be_bytes(bytes: &Self::Bytes) -> Self {
                    <Self as From<$primitive>>::from(<$primitive>::from_be_bytes(*bytes))
                }

                fn from_le_bytes(bytes: &Self::Bytes) -> Self {
                    <Self as From<$primitive>>::from(<$primitive>::from_le_bytes(*bytes))
                }
            }
        )*
    };
}

// Implement ToBytes and FromBytes for all types
impl_to_from_bytes!(
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);

// Macro to implement basic arithmetic operations
macro_rules! impl_binop {
    ($trait:ident, $method:ident, $assign_trait:ident, $assign_method:ident, $($t:ident => $primitive:ty),*) => {
        $(
            impl<E: Endian> $trait for $t<E> {
                type Output = Self;

                fn $method(self, rhs: Self) -> Self::Output {
                    <Self as From<$primitive>>::from(self.to_native().$method(rhs.to_native()))
                }
            }

            impl<E: Endian> $trait<&$t<E>> for $t<E> {
                type Output = Self;

                fn $method(self, rhs: &$t<E>) -> Self::Output {
                    <Self as From<$primitive>>::from(self.to_native().$method(rhs.to_native()))
                }
            }

            impl<E: Endian> $trait<$t<E>> for &$t<E> {
                type Output = $t<E>;

                fn $method(self, rhs: $t<E>) -> Self::Output {
                    <$t<E> as From<$primitive>>::from(self.to_native().$method(rhs.to_native()))
                }
            }

            impl<E: Endian> $trait<&$t<E>> for &$t<E> {
                type Output = $t<E>;

                fn $method(self, rhs: &$t<E>) -> Self::Output {
                    <$t<E> as From<$primitive>>::from(self.to_native().$method(rhs.to_native()))
                }
            }

            impl<E: Endian> $assign_trait for $t<E> {
                fn $assign_method(&mut self, rhs: Self) {
                    *self = <Self as From<$primitive>>::from(self.to_native().$method(rhs.to_native()));
                }
            }

            impl<E: Endian> $assign_trait<&$t<E>> for $t<E> {
                fn $assign_method(&mut self, rhs: &Self) {
                    *self = <Self as From<$primitive>>::from(self.to_native().$method(rhs.to_native()));
                }
            }
        )*
    };
}

// Implement arithmetic operations for all types
impl_binop!(Add, add, AddAssign, add_assign,
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);

impl_binop!(Sub, sub, SubAssign, sub_assign,
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);

impl_binop!(Mul, mul, MulAssign, mul_assign,
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);

impl_binop!(Div, div, DivAssign, div_assign,
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);

impl_binop!(Rem, rem, RemAssign, rem_assign,
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);

// Macro to implement Zero, One, ConstZero, ConstOne traits
macro_rules! impl_zero_one {
    ($($t:ident => $primitive:ty),*) => {
        $(
            impl<E: Endian> Zero for $t<E> {
                fn zero() -> Self {
                    <Self as From<$primitive>>::from(<$primitive>::zero())
                }

                fn is_zero(&self) -> bool {
                    self.to_native().is_zero()
                }
            }

            impl<E: Endian> One for $t<E> {
                fn one() -> Self {
                    <Self as From<$primitive>>::from(<$primitive>::one())
                }
            }

            impl<E: Endian> ConstZero for $t<E> {
                const ZERO: Self = Self::from_native(<$primitive>::ZERO);
            }

            impl<E: Endian> ConstOne for $t<E> {
                const ONE: Self = Self::from_native(<$primitive>::ONE);
            }
        )*
    };
}

impl_zero_one!(
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);

// Macro to implement Display trait
macro_rules! impl_display {
    ($($t:ident => $primitive:ty),*) => {
        $(
            impl<E: Endian> Display for $t<E> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    Display::fmt(&self.to_native(), f)
                }
            }
        )*
    };
}

impl_display!(
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);

// Macro to implement FromStr trait
macro_rules! impl_from_str {
    ($($t:ident => $primitive:ty),*) => {
        $(
            impl<E: Endian> FromStr for $t<E> {
                type Err = <$primitive as FromStr>::Err;

                fn from_str(s: &str) -> Result<Self, Self::Err> {
                    <$primitive>::from_str(s).map(<Self as From<$primitive>>::from)
                }
            }
        )*
    };
}

impl_from_str!(
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);

// Macro to implement ToPrimitive trait
macro_rules! impl_to_primitive {
    ($($t:ident => $primitive:ty),*) => {
        $(
            impl<E: Endian> ToPrimitive for $t<E> {
                fn to_i64(&self) -> Option<i64> {
                    self.to_native().to_i64()
                }

                fn to_u64(&self) -> Option<u64> {
                    self.to_native().to_u64()
                }

                fn to_i32(&self) -> Option<i32> {
                    self.to_native().to_i32()
                }

                fn to_u32(&self) -> Option<u32> {
                    self.to_native().to_u32()
                }

                fn to_i16(&self) -> Option<i16> {
                    self.to_native().to_i16()
                }

                fn to_u16(&self) -> Option<u16> {
                    self.to_native().to_u16()
                }

                fn to_i8(&self) -> Option<i8> {
                    self.to_native().to_i8()
                }

                fn to_u8(&self) -> Option<u8> {
                    self.to_native().to_u8()
                }

                fn to_f32(&self) -> Option<f32> {
                    self.to_native().to_f32()
                }

                fn to_f64(&self) -> Option<f64> {
                    self.to_native().to_f64()
                }
            }
        )*
    };
}

impl_to_primitive!(
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);

// Macro to implement FromPrimitive trait
macro_rules! impl_from_primitive {
    ($($t:ident => $primitive:ty),*) => {
        $(
            impl<E: Endian> FromPrimitive for $t<E> {
                fn from_i64(n: i64) -> Option<Self> {
                    <$primitive>::from_i64(n).map(<Self as From<$primitive>>::from)
                }

                fn from_u64(n: u64) -> Option<Self> {
                    <$primitive>::from_u64(n).map(<Self as From<$primitive>>::from)
                }

                fn from_i32(n: i32) -> Option<Self> {
                    <$primitive>::from_i32(n).map(<Self as From<$primitive>>::from)
                }

                fn from_u32(n: u32) -> Option<Self> {
                    <$primitive>::from_u32(n).map(<Self as From<$primitive>>::from)
                }

                fn from_i16(n: i16) -> Option<Self> {
                    <$primitive>::from_i16(n).map(<Self as From<$primitive>>::from)
                }

                fn from_u16(n: u16) -> Option<Self> {
                    <$primitive>::from_u16(n).map(<Self as From<$primitive>>::from)
                }

                fn from_i8(n: i8) -> Option<Self> {
                    <$primitive>::from_i8(n).map(<Self as From<$primitive>>::from)
                }

                fn from_u8(n: u8) -> Option<Self> {
                    <$primitive>::from_u8(n).map(<Self as From<$primitive>>::from)
                }

                fn from_f32(n: f32) -> Option<Self> {
                    <$primitive>::from_f32(n).map(<Self as From<$primitive>>::from)
                }

                fn from_f64(n: f64) -> Option<Self> {
                    <$primitive>::from_f64(n).map(<Self as From<$primitive>>::from)
                }
            }
        )*
    };
}

impl_from_primitive!(
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);

// Macro to implement checked arithmetic operations
macro_rules! impl_checked_ops {
    ($($t:ident => $primitive:ty),*) => {
        $(
            impl<E: Endian> CheckedAdd for $t<E> {
                fn checked_add(&self, v: &Self) -> Option<Self> {
                    self.to_native().checked_add(v.to_native()).map(<Self as From<$primitive>>::from)
                }
            }

            impl<E: Endian> CheckedSub for $t<E> {
                fn checked_sub(&self, v: &Self) -> Option<Self> {
                    self.to_native().checked_sub(v.to_native()).map(<Self as From<$primitive>>::from)
                }
            }

            impl<E: Endian> CheckedMul for $t<E> {
                fn checked_mul(&self, v: &Self) -> Option<Self> {
                    self.to_native().checked_mul(v.to_native()).map(<Self as From<$primitive>>::from)
                }
            }

            impl<E: Endian> CheckedDiv for $t<E> {
                fn checked_div(&self, v: &Self) -> Option<Self> {
                    self.to_native().checked_div(v.to_native()).map(<Self as From<$primitive>>::from)
                }
            }
        )*
    };
}

impl_checked_ops!(
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);

// Macro to implement saturating operations
macro_rules! impl_saturating {
    ($($t:ident => $primitive:ty),*) => {
        $(
            impl<E: Endian> Saturating for $t<E> {
                fn saturating_add(self, v: Self) -> Self {
                    <Self as From<$primitive>>::from(self.to_native().saturating_add(v.to_native()))
                }

                fn saturating_sub(self, v: Self) -> Self {
                    <Self as From<$primitive>>::from(self.to_native().saturating_sub(v.to_native()))
                }
            }
        )*
    };
}

impl_saturating!(
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);

// Macro to implement wrapping operations
macro_rules! impl_wrapping_ops {
    ($($t:ident => $primitive:ty),*) => {
        $(
            impl<E: Endian> WrappingAdd for $t<E> {
                fn wrapping_add(&self, v: &Self) -> Self {
                    <Self as From<$primitive>>::from(self.to_native().wrapping_add(v.to_native()))
                }
            }

            impl<E: Endian> WrappingSub for $t<E> {
                fn wrapping_sub(&self, v: &Self) -> Self {
                    <Self as From<$primitive>>::from(self.to_native().wrapping_sub(v.to_native()))
                }
            }

            impl<E: Endian> WrappingMul for $t<E> {
                fn wrapping_mul(&self, v: &Self) -> Self {
                    <Self as From<$primitive>>::from(self.to_native().wrapping_mul(v.to_native()))
                }
            }
        )*
    };
}

impl_wrapping_ops!(
    U8 => u8, U16 => u16, U32 => u32, U64 => u64,
    I8 => i8, I16 => i16, I32 => i32, I64 => i64
);
