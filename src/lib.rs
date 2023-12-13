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

    //Macro helper traits
    pub trait __zorua_internal_1 {
        fn get(&self) -> &Self;
    }
    impl __zorua_internal_1 for u8 {
        fn get(&self) -> &Self {
            self
        }
    }
    impl __zorua_internal_1 for u16 {
        fn get(&self) -> &Self {
            self
        }
    }
    impl __zorua_internal_1 for u32 {
        fn get(&self) -> &Self {
            self
        }
    }
    impl __zorua_internal_1 for u64 {
        fn get(&self) -> &Self {
            self
        }
    }
    impl __zorua_internal_1 for u128 {
        fn get(&self) -> &Self {
            self
        }
    }
    pub trait __zorua_internal_2 {
        type backing;
    }
    impl __zorua_internal_2 for u8 {
        type backing = u8;
    }
    impl __zorua_internal_2 for u16_le {
        type backing = u16;
    }
    impl __zorua_internal_2 for u32_le {
        type backing = u32;
    }
    impl __zorua_internal_2 for u64_le {
        type backing = u64;
    }
    impl __zorua_internal_2 for u128_le {
        type backing = u128;
    }
    impl __zorua_internal_2 for u16_be {
        type backing = u16;
    }
    impl __zorua_internal_2 for u32_be {
        type backing = u32;
    }
    impl __zorua_internal_2 for u64_be {
        type backing = u64;
    }
    impl __zorua_internal_2 for u128_be {
        type backing = u128;
    }

    #[macro_export]
    macro_rules! zorua {
        //Subfield get/set implementations
        (impl "$sf_impl", $f:ident, $ft:ty, $sfv:vis, $sf:ident, $sfs:expr, bool) => {
            paste! {
                $sfv fn $sf(&self) -> bool {
                    let bitmask = 1 << $sfs;
                    (self.$f.get() & bitmask) != 0
                }
                $sfv fn [<set_ $sf>](&mut self, val: bool) {
                    let bitmask = 1 << $sfs;
                    if val {
                        self.$f |= Into::<$ft>::into(bitmask);
                    } else {
                        self.$f &= Into::<$ft>::into(!bitmask);
                    }
                }
            }
        };
        (impl "$sf_impl", $f:ident, $ft:ty, $sfv:vis, $sf:ident, $sfs:expr, $sft:ty) => {
            paste! {
                $sfv fn $sf(&self) -> $sft {
                    $sft::new(
                        ((self.$f.get() & (($sft::MASK as <$ft as __zorua_internal_2>::backing) << $sfs)) >> $sfs)
                            as <$sft as Number>::UnderlyingType,
                    )
                }
                $sfv fn [<set_ $sf>](&mut self, val: $sft) {
                    self.$f |= Into::<$ft>::into((val.value() << $sfs) as <$ft as __zorua_internal_2>::backing);
                    self.$f &= Into::<$ft>::into(!(($sft::MASK as <$ft as __zorua_internal_2>::backing) << $sfs));
                }
            }
        };

        //Regular struct macro
        //Generic support courtesy of: https://stackoverflow.com/a/61189128/10910105
        (
            $(#[$struct_meta:meta])*
            $sv:vis struct $struct_name:ident$(<$($lt:tt$(:$clt:tt$(+$dlt:tt)*)?),+>)? {
                $($fv:vis $f:ident : $ft:ty,
                    $($(|$sfv:vis $sf:ident : $sfs:expr;$sft:tt,)+)?
                )*
            };
        ) => {
                // Define the struct
                $(#[$struct_meta])*
                #[derive(
                    zerocopy::AsBytes,
                    zerocopy::FromZeroes,
                    zerocopy::FromBytes,
                    Debug,
                    PartialEq,
                    Clone,
                )]
                $sv struct $struct_name$(<$($lt $(:$clt$(+$dlt)*)?),+>)? {
                    $($fv $f: $ft),*
                }
                // Generate the impl block
                impl$(<$($lt$(:$clt$(+$dlt)*)?),+>)? $struct_name$(<$($lt),+>)? {
                    $($($(
                        zorua!(impl "$sf_impl", $f, $ft, $sfv, $sf, $sfs, $sft);
                    )+)?)*
                }
        };

        //Every other item (which should really just be tuple structs)
        ($item: item) => {
            #[derive(
                zerocopy::AsBytes,
                zerocopy::FromZeroes,
                zerocopy::FromBytes,
                Debug,
                PartialEq,
                Clone,
            )]
            $item
        };
    }
    pub use zorua;
}
