use crate::traits::ZoruaField;

/// An unaligned version of the given type `T`.
/// Note that `T` must:
/// - Implement [`ZoruaField`]
/// - Must be [`Copy`]
#[repr(C, packed)] // Using repr(packed) (unsafe in general) here is fine because T:Copy
#[derive(ZoruaField, Clone, Copy, Debug, PartialEq, Eq)]
#[unsafe_confirm_no_padding] // repr(packed) ensures no padding
pub struct Unaligned<T: ZoruaField + Copy>(T);

impl<T: ZoruaField + Copy> Unaligned<T> {
    pub const fn new(value: T) -> Self {
        Self(value)
    }

    pub fn value(self) -> T {
        self.0
    }
}

impl<T: ZoruaField + Copy> From<T> for Unaligned<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}
