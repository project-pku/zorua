pub mod data_type;
pub mod traits;

#[macro_use]
pub mod prelude {
    pub use crate::data_type::*;
    pub use crate::traits::*;
    pub use paste::paste;

    #[macro_export]
    macro_rules! bitfields {
        //array bitfields
        (impl "subfield_impl", $f:ident, $sfv:vis, $sf:ident, $sfi:literal, [$sft:tt;$sfl:literal],) => {
            paste! {
                $sfv fn $sf(&self, index: usize) -> $sft {
                    let bit_repr = self.$f.get_bits_at::<<$sft as ZoruaBitField>::BitRepr>($sfi+<$sft as ZoruaBitField>::BitRepr::BITS as usize*index);
                    <$sft as ZoruaBitField>::from_bit_repr(bit_repr)
                }
                $sfv fn [<set_ $sf>](&mut self, val: $sft, index: usize) {
                    let bit_repr = val.to_bit_repr();
                    self.$f.set_bits_at::<<$sft as ZoruaBitField>::BitRepr>(bit_repr, $sfi+<$sft as ZoruaBitField>::BitRepr::BITS as usize*index);
                }
            }
        };
        //all other bitfields
        (impl "subfield_impl", $f:ident, $sfv:vis, $sf:ident, $sfi:literal, $sft:tt, $($sftg1:tt, $sftg2:tt)?) => {
            paste! {
                $sfv fn $sf(&self) -> $sft$(<$sftg1, $sftg2>)? {
                    let bit_repr = self.$f.get_bits_at::<<$sft$(<$sftg1, $sftg2>)? as ZoruaBitField>::BitRepr>($sfi);
                    <$sft$(<$sftg1, $sftg2>)? as ZoruaBitField>::from_bit_repr(bit_repr)
                }
                $sfv fn [<set_ $sf>](&mut self, val: $sft$(<$sftg1, $sftg2>)?) {
                    let bit_repr = val.to_bit_repr();
                    self.$f.set_bits_at::<<$sft$(<$sftg1, $sftg2>)? as ZoruaBitField>::BitRepr>(bit_repr, $sfi);
                }
            }
        };

        //Regular struct macro
        //Generic support courtesy of: https://stackoverflow.com/a/61189128/10910105
        (
            $(#[$struct_meta:meta])*
            $sv:vis struct $s:ident$(<$($g:tt$(:$gt:tt$(+$gtx:tt)*)?),+>)? {
                $($fv:vis $f:ident : $ft:ty,
                    $($(|$sfv:vis $sf:ident : $sft:tt$(<$sftg1:tt, $sftg2:tt>)?@$sfi:literal,)+)?
                )*
            }
        ) => {
                // Define the struct
                $(#[$struct_meta])*
                $sv struct $s$(<$($g $(:$gt$(+$gtx)*)?),+>)? {
                    $($fv $f: $ft),*
                }
                // Generate the impl block
                impl$(<$($g$(:$gt$(+$gtx)*)?),+>)? $s$(<$($g),+>)? {
                    $($($(
                        bitfields!(impl "subfield_impl", $f, $sfv, $sf, $sfi, $sft, $($sftg1, $sftg2)?);
                    )+)?)*
                }
        };
    }
    pub use bitfields;

    /// Transmutes a [`ZoruaField`] or [`AlignedBytes`] type into another
    /// `ZoruaField` or `AlignedBytes` type.
    ///
    /// The macro will reject any types that do not satisfy the following:
    /// - The transmuted types must have the same size.
    /// - For `AlignedBytes`, [`AlignedBytes::HAS_PADDING`] must be false.
    ///
    /// An example:
    /// ```
    /// let x = ZfTypeA::new(); //ZfTypeA
    /// let y = transmute!(ZfTypeA, ZfTypeB, x); //ZfTypeB
    /// ```
    ///
    /// ## Note:
    /// The alignment of the `src` and `dst` types are not checked, as this
    /// is unnecessary for transmuting *values* (see [`std::mem::transmute`]).
    #[macro_export]
    macro_rules! transmute {
        ($src:ty, $dst:ty, $val:expr) => {{
            const fn _assert_implements_transmutable<T: Transmutable>() {}

            _assert_implements_transmutable::<$src>(); //in is Transmutable
            _assert_implements_transmutable::<$dst>(); //out is Transmutable
            const _NO_PADDING_SRC: () = assert!(!<$src>::HAS_PADDING); //in has no padding
            const _NO_PADDING_DST: () = assert!(!<$dst>::HAS_PADDING); //out has no padding
            unsafe { std::mem::transmute::<$src, $dst>($val) } //in/out sizes are equal
        }};
    }
    pub use transmute;

    /// A shorthand for [`transmute!(bytes_type!(DST), DST, val)`](macro@crate::transmute).
    #[macro_export]
    macro_rules! transmute_from_bytes {
        ($dst:ty, $val:expr) => {
            transmute!(bytes_type!($dst), $dst, $val)
        };
    }
    pub use transmute_from_bytes;

    /// A shorthand for [`transmute!(SRC, bytes_type!(SRC), val)`](macro@crate::transmute).
    #[macro_export]
    macro_rules! transmute_to_bytes {
        ($src:ty, $val:expr) => {
            transmute!($src, bytes_type!($src), $val)
        };
    }
    pub use transmute_to_bytes;

    /// Transmutes a [Box]ed [`ZoruaField`] or [`AlignedBytes`] type into another
    /// boxed `ZoruaField` or `AlignedBytes` type.
    ///
    /// The macro will reject any types that do not satisfy the following:
    /// - The transmuted types must have the same size.
    /// - The alignment of the `src` type must be >= the alignment of the `dst` type.
    /// - For `AlignedBytes`, [`AlignedBytes::HAS_PADDING`] must be false.
    ///
    /// An example:
    /// ```
    /// let x = Box::new(ZfTypeA::new()); //Box<ZfTypeA>
    /// let y = box_transmute!(ZfTypeA, ZfTypeB, x); //Box<ZfTypeB>
    /// ```
    ///
    /// ## Note:
    /// Unlike [`macro@crate::transmute`], this transmutation is alignment sensitive
    /// due to the indirection intoduced by `Box`.
    #[macro_export]
    macro_rules! box_transmute {
        ($src:ty, $dst:ty, $val:expr) => {{
            const fn _assert_implements_transmutable<T: Transmutable>() {}

            _assert_implements_transmutable::<$src>(); //src is Transmutable
            _assert_implements_transmutable::<$dst>(); //dst is Transmutable
            const _NO_PADDING_SRC: () = assert!(!<$src>::HAS_PADDING); //src has no padding
            const _NO_PADDING_DST: () = assert!(!<$dst>::HAS_PADDING); //dst has no padding

            //types have same size (unlike transmute we have to check it ourselves)
            const _SIZE_CHECK: () =
                assert!(std::mem::size_of::<$src>() == std::mem::size_of::<$dst>());
            //types have compatible alignments (must check due to indirection)
            const _ALIGN_CHECK: () =
                assert!(std::mem::align_of::<$src>() >= std::mem::align_of::<$dst>());

            unsafe {
                let foo_ptr: *mut $src = Box::into_raw($val);
                let bar_ptr: *mut $dst = foo_ptr as *mut $dst;
                Box::from_raw(bar_ptr)
            }
        }};
    }
    pub use box_transmute;

    /// A shorthand for [`box_transmute!(bytes_type!(DST), DST, val)`](macro@crate::box_transmute).
    #[macro_export]
    macro_rules! box_transmute_from_bytes {
        ($dst:ty, $val:expr) => {
            box_transmute!(bytes_type!($dst), $dst, $val)
        };
    }
    pub use box_transmute_from_bytes;

    /// A shorthand for [`box_transmute!(SRC, bytes_type!(SRC), val)`](macro@crate::box_transmute).
    #[macro_export]
    macro_rules! box_transmute_to_bytes {
        ($src:ty, $val:expr) => {
            box_transmute!($src, bytes_type!($src), $val)
        };
    }
    pub use box_transmute_to_bytes;

    /// Shorthand for the [`AlignedBytes`] type with the same size and
    /// alignment as the input type. For example:
    ///
    /// ```
    /// //ALIGN = 2, SIZE = 4
    /// struct MyStruct {
    ///     a: u16,
    ///     b: u16,
    /// }
    /// assert_eq!(
    ///     TypeId::of::<AlignedBytes<2, 4>>>(),
    ///     TypeId::of::<bytes_type!(MyStruct)>()
    /// ); //passes
    /// ```
    #[macro_export]
    macro_rules! bytes_type {
        ($ty:ty) => {
            AlignedBytes<
                { std::mem::align_of::<$ty>() },
                { std::mem::size_of::<$ty>() }
            >
        }
    }
    pub use bytes_type;
}
