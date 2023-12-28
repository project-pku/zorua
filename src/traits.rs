use crate::data_type::*;

/// Automates boilerplate for implementing ZoruaField
/// and BackingField on built-in int types
macro_rules! impl_backing {
    ($($ty:ty),*) => {
        $(
            impl BackingField for $ty {
                fn get_bits<T: BackingBitField>(self, index: usize) -> T
                where Self: From<T::ByteRepr> + TryInto<T::ByteRepr> {
                    T::from_backed(
                        ((self & (Into::<$ty>::into(T::MASK) << index)) >> index).try_into().unwrap_or_else(
                            |_| {
                                panic!("Zorua Error: The BitRepr::MASK of type {} must be wrong",
                                    std::any::type_name::<$ty>())
                            }
                        )
                    )
                }
                fn set_bits<T: BackingBitField>(&mut self, value: T, index: usize)
                where Self: From<T::ByteRepr> + TryInto<T::ByteRepr> {
                    *self &= !(Into::<$ty>::into(T::MASK) << index);
                    *self |= (Into::<$ty>::into(value.to_backed())) << index;
                }
            }
            impl ZoruaField for $ty {
                fn swap_bytes_mut(&mut self) {
                    *self = (*self).swap_bytes();
                }
            }
        )*
    };
}

/// Automates boilerplate for implementing ZoruaBitField
/// and BackingBitField on arbitrary-int & built-in int types
macro_rules! impl_bit_backing {
    ("arbitrary", $($ty:ty),*) => {
        $(
            impl ZoruaBitField for $ty {
                type BitRepr = Self;
                fn to_repr(self) -> Self::BitRepr { self }
                fn from_repr(value: Self::BitRepr) -> Self { value }
            }
            impl BackingBitField for $ty {
                type ByteRepr = <$ty as Number>::UnderlyingType;
                const MASK: Self::ByteRepr = <$ty>::MASK;
                fn to_backed(self) -> Self::ByteRepr {
                    self.value()
                }
                fn from_backed(value: Self::ByteRepr) -> Self {
                    Self::new(value)
                }
            }
        )*
    };
    ("native", $($ty:ty),*) => {
        $(
            impl ZoruaBitField for $ty {
                type BitRepr = Self;
                fn to_repr(self) -> Self::BitRepr { self }
                fn from_repr(value: Self::BitRepr) -> Self { value }
            }
            impl BackingBitField for $ty {
                type ByteRepr = Self;
                const MASK: Self::ByteRepr = Self::MAX;
                fn to_backed(self) -> Self::ByteRepr { self }
                fn from_backed(value: Self::ByteRepr) -> Self { value }
            }
        )*
    };
}

//------------- Field trait + impls -------------
/// A type that can be used within a ZoruaStruct.
pub trait ZoruaField {
    fn swap_bytes_mut(&mut self);

    fn to_le_mut(&mut self) {
        //Assumption: All targets are either little or big endian.
        #[cfg(target_endian = "big")]
        {
            self.swap_bytes_mut();
        }
    }

    fn to_be_mut(&mut self) {
        //Assumption: All targets are either little or big endian.
        #[cfg(target_endian = "little")]
        {
            self.swap_bytes_mut();
        }
    }
}

/// A special kind of [ZoruaField] that can house [ZoruaBitField]s.
pub trait BackingField: ZoruaField + Copy + std::fmt::Debug + PartialEq {
    fn get_bits<T: BackingBitField>(self, index: usize) -> T
    where
        Self: From<T::ByteRepr> + TryInto<T::ByteRepr>;

    fn set_bits<T: BackingBitField>(&mut self, value: T, index: usize)
    where
        Self: From<T::ByteRepr> + TryInto<T::ByteRepr>;
}

impl_backing!(u8, u16, u32, u64, u128);

impl ZoruaField for () {
    fn swap_bytes_mut(&mut self) {}
}

impl<const N: usize, T: ZoruaField> ZoruaField for [T; N] {
    fn swap_bytes_mut(&mut self) {
        self.iter_mut().for_each(|value| {
            value.swap_bytes_mut();
        });
    }
}

//----------- BitField trait + impls -----------
pub trait ZoruaBitField {
    /// The smallest bit representation of this type.
    type BitRepr: BackingBitField;

    /// Returns a copy of this type as its N bit [BitRepr][ZoruaBitField].
    fn to_repr(self) -> Self::BitRepr;

    /// Packs this type into its N bit [BitRepr][ZoruaBitField].
    fn from_repr(value: Self::BitRepr) -> Self;
}

pub trait BackingBitField: ZoruaBitField + Copy {
    type ByteRepr: BackingField;
    const MASK: Self::ByteRepr;
    fn to_backed(self) -> Self::ByteRepr;
    fn from_backed(value: Self::ByteRepr) -> Self;
}

impl_bit_backing!("arbitrary", u1, u2, u3, u4, u5, u6, u7);
impl_bit_backing!("native", u8, u16, u32, u64, u128);

impl ZoruaBitField for bool {
    type BitRepr = u1;
    fn to_repr(self) -> Self::BitRepr {
        u1::new(if self { 1 } else { 0 })
    }
    fn from_repr(value: Self::BitRepr) -> Self {
        value == u1::MAX
    }
}
