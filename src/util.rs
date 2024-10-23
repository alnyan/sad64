use std::fmt;

macro_rules! impl_number_like {
    ($($ty:ty),+) => {
        $(
            impl NumberLike for $ty {
                const ZERO: Self = 0;

                fn absolute(&self) -> Self {
                    self.abs()
                }

                fn sign(&self) -> bool {
                    *self >= 0
                }
            }
        )+
    };
}

pub trait NumberLike {
    const ZERO: Self;

    fn absolute(&self) -> Self;
    fn sign(&self) -> bool;
}

pub struct Signed<T: Ord + NumberLike>(pub T);

impl<T: Ord + NumberLike + fmt::LowerHex> fmt::LowerHex for Signed<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let prefix = if f.alternate() { "0x" } else { "" };
        let bare_hex = format!("{:x}", self.0.absolute());
        f.pad_integral(self.0.sign(), prefix, &bare_hex)
    }
}

impl<T: Ord + NumberLike + fmt::UpperHex> fmt::UpperHex for Signed<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let prefix = if f.alternate() { "0x" } else { "" };
        let bare_hex = format!("{:X}", self.0.absolute());
        f.pad_integral(self.0.sign(), prefix, &bare_hex)
    }
}

impl_number_like!(i8, i16, i32, i64);
