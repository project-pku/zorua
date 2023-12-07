pub mod int {
    pub use arbitrary_int::*;
    pub use rkyv::rend::{
        char_be, char_le,
        f32_be, f32_le, f64_be, f64_le,
        i16_be, i32_be, i64_be, i128_be,
        i16_le, i32_le, i64_le, i128_le,
        u16_be, u32_be, u64_be, u128_be,
        u16_le, u32_le, u64_le, u128_le,
        NonZeroI16_be, NonZeroI32_be, NonZeroI64_be, NonZeroI128_be,
        NonZeroI16_le, NonZeroI32_le, NonZeroI64_le, NonZeroI128_le,
        NonZeroU16_be, NonZeroU32_be, NonZeroU64_be, NonZeroU128_be,
        NonZeroU16_le, NonZeroU32_le, NonZeroU64_le, NonZeroU128_le,
    };
}

#[macro_use]
pub mod prelude {
    pub use rkyv::Archive;
    pub use crate::int::*;
    pub use paste::paste;

    //TODO: currently unable to have bitfields in a different size range...
    #[macro_export]
    macro_rules! zorua {
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
            $sv:vis $struct_name:ident {
            $($fv:vis $f:ident : $ft:ty,
                $($(|$sfv:vis $sf:ident : $sfs:expr;$sft:tt,)+)?
            )*
        }) => {
                // Define the struct
                #[repr(C)]
                $(#[$struct_meta])*
                $sv struct $struct_name {
                    $($fv $f: $ft),*
                }
                // Generate the impl block
                impl $struct_name {
                    $($($(
                        zorua!(impl "$sf_impl", $f, $sfv, $sf, $sfs, $sft);
                    )+)?)*
                }
        };
    }
    pub use zorua;
}