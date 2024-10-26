use core::fmt;

use crate::{IndexMode, Instruction, Operand};

macro_rules! impl_number_like {
    ($($ty:ty),+) => {
        $(
            impl NumberLike for $ty {
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

trait NumberLike {
    fn absolute(&self) -> Self;
    fn sign(&self) -> bool;
}

struct Signed<T: Ord + NumberLike>(pub T);

/// Trait for printing instructions.
pub trait Formatter {
    /// Writes the instruction to output `f`.
    fn write_insn<W: fmt::Write, R: SymbolResolver>(
        &mut self,
        f: &mut W,
        pc: u64,
        resolver: &R,
        insn: &Instruction,
    ) -> fmt::Result;
}

/// Trait for resolving addresses to their respective symbols in the binary.
pub trait SymbolResolver {
    /// Given an address, returns a symbol it refers to, as well as an offset within the symbol.
    fn resolve(&self, address: u64) -> Option<(&str, u64)>;
}

/// Basic instruction formatter, prints instrction followed by its operands.
/// Uses [SymbolResolver] to resolve pc-relative addresses to symbol names and offsets.
pub struct SimpleFormatter;
/// Dummy symbol resolver.
pub struct NoopResolver;

/// Helper type to print instructions to [std::io::Stdout].
#[cfg(feature = "std")]
pub struct StdoutOutput;

impl Formatter for SimpleFormatter {
    fn write_insn<W: fmt::Write, R: SymbolResolver>(
        &mut self,
        f: &mut W,
        pc: u64,
        resolver: &R,
        insn: &Instruction,
    ) -> fmt::Result {
        write!(f, "{} ", insn.mnemonic)?;

        for (i, op) in insn.operands.iter().filter_map(|x| *x).enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }

            match op {
                Operand::PcRelImm(imm) => {
                    let address = pc.wrapping_add_signed(imm);

                    match resolver.resolve(address) {
                        Some((location, 0)) => write!(f, "{:#x} <{}>", address, location)?,
                        Some((location, offset)) => {
                            write!(f, "{:#x} <{} + {:#x}>", address, location, offset)?
                        }
                        None => write!(f, "{:#x}", address)?,
                    }
                }
                Operand::Adrp(imm) => {
                    let address = (pc.wrapping_add_signed(imm)) & !0xFFF;

                    match resolver.resolve(address) {
                        Some((location, 0)) => write!(f, "{:#x} <{}>", address, location)?,
                        Some((location, offset)) => {
                            write!(f, "{:#x} <{} + {:#x}>", address, location, offset)?
                        }
                        None => write!(f, "{:#x}", address)?,
                    }
                }

                Operand::DecImm(imm) => write!(f, "#{}", imm)?,
                #[cfg(feature = "std")]
                Operand::FpImm(imm) => write!(f, "#{:?}", imm)?,
                Operand::FpImmUnknown => write!(f, "#<float>")?,
                Operand::FpZero => write!(f, "#0.0")?,
                Operand::Imm(imm) => write!(f, "#{:#x}", imm)?,
                Operand::Simm(imm) => write!(f, "#{:#x}", Signed(imm))?,
                Operand::Lsl(x) => write!(f, "lsl #{}", x)?,
                Operand::Msl(x) => write!(f, "msl #{}", x)?,
                Operand::Lsr(x) => write!(f, "lsr #{}", x)?,
                Operand::Asr(x) => write!(f, "asr #{}", x)?,
                Operand::Ror(x) => write!(f, "ror #{}", x)?,

                // Register
                Operand::XSp(31) => write!(f, "sp")?,
                Operand::XSp(x) => write!(f, "x{}", x)?,

                Operand::WSp(31) => write!(f, "wsp")?,
                Operand::WSp(x) => write!(f, "w{}", x)?,

                Operand::X(31) => write!(f, "xzr")?,
                Operand::X(x) => write!(f, "x{}", x)?,
                Operand::W(31) => write!(f, "wzr")?,
                Operand::W(x) => write!(f, "w{}", x)?,

                // SIMD register
                Operand::VMulti(v) => write!(f, "{}", v)?,
                Operand::VSingle(v) => write!(f, "{}", v)?,
                Operand::VSingleIndex(v, i) => write!(f, "{}[{}]", v, i)?,
                Operand::VMultiGroup(grp) => write!(f, "{}", grp)?,
                Operand::VSingleGroup(grp) => write!(f, "{}", grp)?,

                // Memory
                Operand::MemXSpOff(x, off) => {
                    write!(f, "[")?;
                    if x == 31 {
                        write!(f, "sp")?;
                    } else {
                        write!(f, "x{}", x)?;
                    }

                    match off {
                        IndexMode::WExt(x, ext) if ext.is_displayed() => {
                            write!(f, ", w{}, {}]", x, ext)?
                        }
                        IndexMode::WExt(x, _) => write!(f, ", w{}]", x)?,
                        IndexMode::XExt(x, ext) if ext.is_displayed() => {
                            write!(f, ", x{}, {}]", x, ext)?
                        }
                        IndexMode::XExt(x, _) => write!(f, ", x{}]", x)?,
                        IndexMode::X(x) => write!(f, ", x{}]", x)?,
                        IndexMode::Signed(0) | IndexMode::Unsigned(0) => write!(f, "]")?,
                        IndexMode::Signed(x) => write!(f, ", #{:#x}]", Signed(x))?,
                        IndexMode::Unsigned(x) => write!(f, ", #{:#x}]", x)?,
                        IndexMode::PreIndex(x) => write!(f, ", #{:#x}]!", Signed(x))?,
                    }
                }

                Operand::RegExtend(ext) => write!(f, "{}", ext)?,
                Operand::Barrier(b) => write!(f, "{}", b)?,
                Operand::Sys(s) => write!(f, "{}", s)?,
                Operand::SysC(n, m) => write!(f, "c{}, c{}", n, m)?,
                Operand::SysOp(t) => write!(f, "{}", t)?,
                Operand::Daifset => write!(f, "daifset")?,
                Operand::Daifclr => write!(f, "daifclr")?,
                Operand::Spsel => write!(f, "spsel")?,
                Operand::Prefetch(prfm) => write!(f, "{}", prfm)?,

                Operand::Cond(cond) => write!(f, "{:?}", cond)?,
            }
        }

        Ok(())
    }
}

impl SymbolResolver for NoopResolver {
    fn resolve(&self, _address: u64) -> Option<(&str, u64)> {
        None
    }
}

impl<T: Ord + NumberLike + fmt::LowerHex> fmt::LowerHex for Signed<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.sign() {
            false => {
                f.write_str("-")?;
                fmt::LowerHex::fmt(&self.0.absolute(), f)
            }
            true => fmt::LowerHex::fmt(&self.0, f),
        }
    }
}

#[cfg(feature = "std")]
impl fmt::Write for StdoutOutput {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        print!("{}", s);
        Ok(())
    }
}

impl_number_like!(i8, i16, i32, i64);
