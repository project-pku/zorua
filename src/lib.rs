#![allow(non_camel_case_types)]

pub mod transmute {
    pub use zerocopy::{AsBytes, FromBytes, FromZeroes};
}

pub mod data_type {
    pub use arbitrary_int::*;

    // Little Endian
    use zerocopy::little_endian;
    pub type u16_le = little_endian::U16;
    pub type u32_le = little_endian::U32;
    pub type u64_le = little_endian::U64;
    pub type u128_le = little_endian::U128;
    pub type i16_le = little_endian::I16;
    pub type i32_le = little_endian::I32;
    pub type i64_le = little_endian::I64;
    pub type i128_le = little_endian::I128;
    pub type f32_le = little_endian::F32;
    pub type f64_le = little_endian::F64;

    // Big Endian
    use zerocopy::big_endian;
    pub type u16_be = big_endian::U16;
    pub type u32_be = big_endian::U32;
    pub type u64_be = big_endian::U64;
    pub type u128_be = big_endian::U128;
    pub type i16_be = big_endian::I16;
    pub type i32_be = big_endian::I32;
    pub type i64_be = big_endian::I64;
    pub type i128_be = big_endian::I128;
    pub type f32_be = big_endian::F32;
    pub type f64_be = big_endian::F64;
}

#[macro_use]
pub mod prelude {
    pub use crate::data_type::*;
    pub use crate::transmute::*;
    pub use paste::paste;

    #[macro_export]
    macro_rules! zorua_wrapper {
        ($item: item) => {
            #[repr(transparent)]
            #[derive(
                zerocopy::AsBytes, zerocopy::FromZeroes, zerocopy::FromBytes, Debug, PartialEq,
            )]
            $item
        };
    }
    pub use zorua_wrapper;

    //TODO: currently unable to have bitfields in a different size range...
    #[macro_export]
    macro_rules! zorua_w_bitfields {
        (impl "$sf_impl", $f:ident, $sfv:vis, $sf:ident, $sfs:expr, bool) => {
            paste! {
                $sfv fn $sf(&self) -> bool {
                    let bitmask = 1 << $sfs;
                    (self.$f & bitmask) != 0
                }
                $sfv fn [<set_ $sf>](&mut self, val: bool) {
                    let bitmask = 1 << $sfs;
                    if val {
                        self.$f |= bitmask;
                    } else {
                        self.$f &= !bitmask;
                    }
                }
            }
        };
        (impl "$sf_impl", $f:ident, $sfv:vis, $sf:ident, $sfs:expr, $sft:ty) => {
            paste! {
                $sfv fn $sf(&self) -> $sft {
                    $sft::new((self.$f & $sft::MASK << $sfs) >> $sfs)
                }
                $sfv fn [<set_ $sf>](&mut self, val: $sft) {
                    self.$f &= !($sft::MASK << $sfs);
                    self.$f |= val.value() << $sfs;
                }
            }
        };
        (
            $(#[$struct_meta:meta])*
            $sv:vis struct $struct_name:ident {
                $($fv:vis $f:ident : $ft:ty,
                    $($(|$sfv:vis $sf:ident : $sfs:expr;$sft:tt,)+)?
                )*
            };
        ) => {
                // Define the struct
                #[repr(C)]
                #[derive(
                    zerocopy::AsBytes,
                    zerocopy::FromZeroes,
                    zerocopy::FromBytes,
                    Debug,
                    PartialEq
                )]
                $(#[$struct_meta])*
                $sv struct $struct_name {
                    $($fv $f: $ft),*
                }
                // Generate the impl block
                impl $struct_name {
                    $($($(
                        zorua_w_bitfields!(impl "$sf_impl", $f, $sfv, $sf, $sfs, $sft);
                    )+)?)*
                }
        };
    }
    pub use zorua_w_bitfields;
}
