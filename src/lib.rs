#![cfg_attr(not(feature = "std"), no_std)]

pub mod data_type;
pub mod native;
pub mod traits;

#[macro_use]
pub mod prelude {
    pub use crate::data_type::*;
    pub use crate::native::ZoruaNative;
    pub use crate::traits::*;
    pub use paste::paste;

    pub use zorua_macro::bitfields;

    /// Shorthand for the [`AlignedBytes`] type with the same size and
    /// alignment as the input type. For example:
    ///
    /// ```
    /// use std::any::TypeId;
    /// use zorua::prelude::*;
    ///
    /// //ALIGN = 2, SIZE = 4
    /// struct MyStruct {
    ///     a: u16,
    ///     b: u16,
    /// }
    /// assert_eq!(
    ///     TypeId::of::<AlignedBytes<2, 4>>(),
    ///     TypeId::of::<aligned_bytes_of!(MyStruct)>()
    /// ); //passes
    /// ```
    #[macro_export]
    macro_rules! aligned_bytes_of {
        ($ty:ty) => {
            AlignedBytes<
                { core::mem::align_of::<$ty>() },
                { core::mem::size_of::<$ty>() }
            >
        }
    }
    pub use aligned_bytes_of;
}
