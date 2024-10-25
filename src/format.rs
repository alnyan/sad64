use std::{borrow::Cow, fmt};

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

pub trait Formatter {
    fn write_insn<R: SymbolResolver>(&mut self, pc: u64, resolver: &R, insn: &Instruction);
}

pub trait SymbolResolver {
    fn resolve(&self, address: u64) -> Option<(Cow<str>, u64)>;
}

pub struct SimpleFormatter;
pub struct NoopResolver;

impl Formatter for SimpleFormatter {
    fn write_insn<R: SymbolResolver>(&mut self, pc: u64, resolver: &R, insn: &Instruction) {
        print!("{} ", insn.mnemonic);

        for (i, op) in insn.operands.iter().filter_map(|x| *x).enumerate() {
            if i != 0 {
                print!(", ");
            }

            match op {
                Operand::PcRelImm(imm) => {
                    let address = pc.wrapping_add_signed(imm);

                    match resolver.resolve(address) {
                        Some((location, 0)) => print!("{:#x} <{}>", address, location),
                        Some((location, offset)) => {
                            print!("{:#x} <{} + {:#x}>", address, location, offset)
                        }
                        None => print!("{:#x}", address),
                    }
                }
                Operand::Adrp(imm) => {
                    let address = (pc.wrapping_add_signed(imm)) & !0xFFF;

                    match resolver.resolve(address) {
                        Some((location, 0)) => print!("{:#x} <{}>", address, location),
                        Some((location, offset)) => {
                            print!("{:#x} <{} + {:#x}>", address, location, offset)
                        }
                        None => print!("{:#x}", address),
                    }
                }

                Operand::DecImm(imm) => print!("#{}", imm),
                Operand::FpImm(imm) => print!("#{:?}", imm),
                Operand::Imm(imm) => print!("#{:#x}", imm),
                Operand::Simm(imm) => print!("#{:#x}", Signed(imm)),
                Operand::Lsl(x) => print!("lsl #{}", x),
                Operand::Lsr(x) => print!("lsr #{}", x),
                Operand::Asr(x) => print!("asr #{}", x),
                Operand::Ror(x) => print!("ror #{}", x),

                // Register
                Operand::XSp(31) => print!("sp"),
                Operand::XSp(x) => print!("x{}", x),

                Operand::WSp(31) => print!("wsp"),
                Operand::WSp(x) => print!("w{}", x),

                Operand::X(31) => print!("xzr"),
                Operand::X(x) => print!("x{}", x),
                Operand::W(31) => print!("wzr"),
                Operand::W(x) => print!("w{}", x),

                // SIMD register
                Operand::VMulti(v) => print!("{}", v),
                Operand::VSingle(v) => print!("{}", v),
                Operand::VSingleIndex(v, i) => print!("{}[{}]", v, i),
                Operand::VMultiGroup(grp) => print!("{}", grp),
                Operand::VSingleGroup(grp) => print!("{}", grp),

                // Memory
                Operand::MemXSpOff(x, off) => {
                    print!("[");
                    if x == 31 {
                        print!("sp");
                    } else {
                        print!("x{}", x);
                    }

                    match off {
                        IndexMode::WExt(x, ext) => {
                            if ext.is_displayed() {
                                print!(", w{}, {}]", x, ext);
                            } else {
                                print!(", w{}]", x);
                            }
                        }
                        IndexMode::XExt(x, ext) => {
                            if ext.is_displayed() {
                                print!(", x{}, {}]", x, ext);
                            } else {
                                print!(", x{}]", x);
                            }
                        }
                        IndexMode::X(x) => print!(", x{}]", x),
                        IndexMode::Signed(0) | IndexMode::Unsigned(0) => print!("]"),
                        IndexMode::Signed(x) => print!(", #{:#x}]", Signed(x)),
                        IndexMode::Unsigned(x) => print!(", #{:#x}]", x),
                        IndexMode::PreIndex(x) => print!(", #{:#x}]!", Signed(x)),
                    }
                }

                Operand::RegExtend(ext) => {
                    print!("{}", ext);
                }

                Operand::Barrier(b) => print!("{}", b),
                Operand::Sys(s) => print!("{}", s),
                Operand::SysC(n, m) => print!("c{}, c{}", n, m),
                Operand::SysOp(t) => print!("{}", t),
                Operand::Daifset => print!("daifset"),
                Operand::Daifclr => print!("daifclr"),
                Operand::Spsel => print!("spsel"),
                Operand::Prefetch(prfm) => print!("{}", prfm),

                Operand::Cond(cond) => {
                    print!("{:?}", cond)
                }
            }
        }
        println!();
    }
}

impl SymbolResolver for NoopResolver {
    fn resolve(&self, _address: u64) -> Option<(Cow<str>, u64)> {
        None
    }
}

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
