#![allow(non_snake_case, non_camel_case_types)]
#![feature(if_let_guard)]

use std::fmt;

use msr::SystemReg;
use util::Signed;

mod decode;
pub mod msr;
pub mod util;

pub trait Formatter {
    fn write_insn(&mut self, insn: &Instruction);
}

pub struct SimpleFormatter;

#[derive(Debug, Clone, Copy)]
pub enum BranchCondition {
    Eq,
    Ne,
    Cs,
    Cc,
    Hi,
    Ls,
    Ge,
    Lt,
    Gt,
    Le,
    Mi,
    Pl,
    Vs,
    Vc,
    Al,
    Nv,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RegExtendWord {
    uxtb,
    uxth,
    uxtw,
    uxtx,
    sxtb,
    sxth,
    sxtw,
    sxtx,
    lsl,
}

#[derive(Debug, Clone, Copy)]
pub struct RegExtend {
    pub word: RegExtendWord,
    pub amount: u8,
}

#[derive(Debug, Clone, Copy)]
pub enum Mnemonic {
    adc,
    adcs,
    add,
    adds,
    adr,
    adrp,
    and,
    ands,
    asr,
    asrv,
    b,
    bfi,
    bfm,
    bfxil,
    bic,
    bics,
    bl,
    blr,
    br,
    brk,
    cbnz,
    cbz,
    ccmn,
    ccmp,
    cinc,
    clrex,
    clz,
    cmn,
    cmp,
    csel,
    cset,
    csinc,
    csinv,
    csneg,
    dmb,
    dsb,
    eor,
    eret,
    extr,
    hvc,
    isb,
    ldar,
    ldarb,
    ldarh,
    ldaxr,
    ldaxrb,
    ldaxrh,
    ldp,
    ldr,
    ldrb,
    ldrh,
    ldrsb,
    ldrsh,
    ldrsw,
    ldur,
    ldurb,
    ldurh,
    ldursb,
    ldxr,
    ldxrb,
    ldxrh,
    lsl,
    lslv,
    lsr,
    madd,
    mneg,
    mov,
    movk,
    movn,
    movz,
    mrs,
    msr,
    msub,
    mul,
    mvn,
    nop,
    orn,
    orr,
    rbit,
    ret,
    rev,
    rev16,
    ror,
    rorv,
    sbc,
    sbcs,
    sbfm,
    sev,
    sdiv,
    smaddl,
    smc,
    smsubl,
    smulh,
    smull,
    stlr,
    stlrb,
    stlrh,
    stlxr,
    stlxrb,
    stlxrh,
    stp,
    str,
    strb,
    strh,
    stur,
    sturb,
    sturh,
    stxr,
    stxrb,
    stxrh,
    sub,
    subs,
    svc,
    sxtb,
    sxth,
    sxtw,
    tbnz,
    tbz,
    tlbi,
    tst,
    ubfiz,
    ubfm,
    ubfx,
    udf,
    udiv,
    umaddl,
    umsubl,
    umul,
    umulh,
    wfe,
    wfi,
    CondB(BranchCondition),
}

#[derive(Debug, Clone, Copy)]
pub enum Tlbi {
    alle1,
    alle1is,
    alle2,
    alle2is,
    alle3,
    alle3is,
    aside1,
    aside1is,
    ipas2e1,
    ipas2e1is,
    ipas2le1,
    ipas2le1is,
    vaae1,
    vaae1is,
    vaale1,
    vaale1is,
    vae1,
    vae2,
    vae3,
    vale1,
    vale1is,
    vale2,
    vale2is,
    vale3,
    vale3is,
    vmalle1,
    vmalle1is,
    vmalls12e1,
    vmalls12e1is,
}

#[derive(Debug, Clone, Copy)]
pub enum SysOp {
    At,
    Dc,
    Ic,
    Tlbi(Tlbi),
}

#[derive(Debug, Clone, Copy)]
pub enum BarrierDomain {
    OuterShareable,
    Nonshareable,
    InnerShareable,
    FullSystem,
}

#[derive(Debug, Clone, Copy)]
pub enum Barrier {
    Reads(BarrierDomain),
    Writes(BarrierDomain),
    All(BarrierDomain),
}

#[derive(Debug, Clone, Copy)]
pub enum IndexMode {
    PreIndex(i64),

    Unsigned(u64),
    Signed(i64),

    X(u8),

    WExt(u8, RegExtend),
    XExt(u8, RegExtend),
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    W(u8),
    X(u8),
    XSp(u8),
    WSp(u8),

    Imm(u64),
    Simm(i64),
    PcRelImm(i64),

    MemXSpOff(u8, IndexMode),

    Lsl(u8),
    Lsr(u8),
    Asr(u8),
    Ror(u8),

    RegExtend(RegExtend),

    Barrier(Barrier),
    Sys(SystemReg),
    Tlbi(Tlbi),

    Cond(BranchCondition),
}

#[derive(Debug)]
pub struct Instruction {
    mnemonic: Mnemonic,
    operands: [Option<Operand>; 4],
}

impl Formatter for SimpleFormatter {
    fn write_insn(&mut self, insn: &Instruction) {
        print!("{} ", insn.mnemonic);

        for (i, op) in insn.operands.iter().filter_map(|x| *x).enumerate() {
            if i != 0 {
                print!(", ");
            }

            match op {
                Operand::PcRelImm(imm) => print!("pc{:+#x}", Signed(imm)),
                Operand::Imm(imm) => print!("#{:#x}", imm),
                Operand::Simm(imm) => print!("#{:#x}", Signed(imm)),
                Operand::Lsl(x) => print!("lsl #{}", x),
                Operand::Lsr(x) => print!("lsr #{}", x),
                Operand::Asr(x) => print!("asr #{}", x),
                Operand::Ror(x) => print!("ror #{}", x),

                // Register
                Operand::XSp(31) => print!("sp"),
                Operand::XSp(x) => print!("x{}", x),

                Operand::WSp(x) => print!("w{}", x),

                Operand::X(31) => print!("xzr"),
                Operand::X(x) => print!("x{}", x),
                Operand::W(31) => print!("wzr"),
                Operand::W(x) => print!("w{}", x),

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
                Operand::Tlbi(t) => print!("{:?}", t),

                Operand::Cond(cond) => {
                    print!("{}", cond)
                }
            }
        }
        println!();
    }
}

impl fmt::Display for Barrier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let label = match self {
            Self::All(BarrierDomain::FullSystem) => "sy",
            Self::Writes(BarrierDomain::FullSystem) => "st",
            Self::All(BarrierDomain::InnerShareable) => "ish",
            Self::Reads(BarrierDomain::InnerShareable) => "ishld",
            _ => todo!(),
        };

        f.write_str(label)
    }
}

impl fmt::Display for Mnemonic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CondB(br) => f.write_fmt(format_args!("b.{}", br)),
            _ => fmt::Debug::fmt(self, f),
        }
    }
}

macro_rules! reg_extend_cons {
    ($($word:ident),+) => {
        $(
            pub fn $word(amount: u8) -> Self {
                Self {
                    word: RegExtendWord::$word,
                    amount,
                }
            }
        )+
    };
}
impl RegExtend {
    reg_extend_cons!(uxtb, uxth, uxtw, uxtx, sxtb, sxtw, sxth, sxtx, lsl);

    pub fn is_displayed(&self) -> bool {
        self.word != RegExtendWord::lsl || self.amount != 0
    }

    pub fn decode(option: u8, amount: u8) -> Self {
        match option {
            0 => Self::uxtb(amount),
            1 => Self::uxth(amount),
            2 => Self::uxtw(amount),
            3 => Self::uxtx(amount),
            4 => Self::sxtb(amount),
            5 => Self::sxth(amount),
            6 => Self::sxtw(amount),
            7 => Self::sxtx(amount),
            _ => unreachable!(),
        }
    }
}

impl TryFrom<u8> for BranchCondition {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Eq),
            1 => Ok(Self::Ne),
            2 => Ok(Self::Cs),
            3 => Ok(Self::Cc),
            4 => Ok(Self::Mi),
            5 => Ok(Self::Pl),
            6 => Ok(Self::Vs),
            7 => Ok(Self::Vc),
            8 => Ok(Self::Hi),
            9 => Ok(Self::Ls),
            10 => Ok(Self::Ge),
            11 => Ok(Self::Lt),
            12 => Ok(Self::Gt),
            13 => Ok(Self::Le),
            14 => Ok(Self::Al),
            15 => Ok(Self::Nv),
            _ => Err(()),
        }
    }
}

impl fmt::Display for RegExtend {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        assert!(self.is_displayed());
        fmt::Debug::fmt(&self.word, f)?;
        if self.amount != 0 {
            f.write_fmt(format_args!(" #{}", self.amount))
        } else {
            Ok(())
        }
    }
}

impl fmt::Display for BranchCondition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let n = match self {
            Self::Eq => "eq",
            Self::Ne => "ne",
            Self::Cs => "cs",
            Self::Cc => "cc",
            Self::Hi => "hi",
            Self::Ls => "ls",
            Self::Ge => "ge",
            Self::Lt => "lt",
            Self::Gt => "gt",
            Self::Le => "le",
            Self::Mi => "mi",
            Self::Pl => "pl",
            Self::Vs => "vs",
            Self::Vc => "vc",
            Self::Al => "al",
            Self::Nv => "nv",
        };
        f.write_str(n)
    }
}

pub fn decode(insn: u32) -> Option<Instruction> {
    decode::decode_inner(insn)
}
