pub mod data_type {
    pub use arbitrary_int::*;
}

#[macro_use]
pub mod prelude {
    pub use crate::data_type::*;
    pub use paste::paste;

    #[macro_export]
    macro_rules! zorua {
        //Subfield get/set implementations
        (impl "$sf_impl", $f:ident, $ft:ty, $sfv:vis, $sf:ident, $sfs:expr, bool) => {
            paste! {
                $sfv fn $sf(&self) -> bool {
                    let bitmask = 1 << $sfs;
                    (self.$f & bitmask) != 0
                }
                $sfv fn [<set_ $sf>](&mut self, val: bool) {
                    let bitmask: $ft = 1 << $sfs;
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
                        ((self.$f & (($sft::MASK as $ft) << $sfs)) >> $sfs)
                            as <$sft as Number>::UnderlyingType,
                    )
                }
                $sfv fn [<set_ $sf>](&mut self, val: $sft) {
                    self.$f &= Into::<$ft>::into(!(($sft::MASK as $ft) << $sfs));
                    self.$f |= Into::<$ft>::into((val.value() << $sfs) as $ft);
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
                #[derive(Debug, PartialEq, Clone)]
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
            #[derive(Debug, PartialEq, Clone)]
            $item
        };
    }
    pub use zorua;
}
