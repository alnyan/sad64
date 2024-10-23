use core::fmt;

use crate::SystemReg;

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    W(u8),
    X(u8),
    XSp(u8),
    WSp(u8),

    Imm(u64),
    Simm(i64),
    PcRelImm(i64),
    Adrp(i64),

    MemXSpOff(u8, IndexMode),

    Lsl(u8),
    Lsr(u8),
    Asr(u8),
    Ror(u8),

    RegExtend(RegExtend),

    Barrier(Barrier),
    Sys(SystemReg),
    SysC(u8, u8),
    SysOp(SysOp),
    Daifset,
    Daifclr,
    Spsel,
    Prefetch(Prefetch),

    Cond(BranchCondition),
}

#[derive(Debug, Clone, Copy)]
pub enum BranchCondition {
    eq,
    ne,
    hs,
    lo,
    hi,
    ls,
    ge,
    lt,
    gt,
    le,
    mi,
    pl,
    vs,
    vc,
    al,
    nv,
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
pub enum At {
    s1e1r,
    s1e2r,
    s1e3r,
    s1e1w,
    s1e2w,
    s1e3w,
    s1e0r,
    s1e0w,
    s12e1r,
    s12e1w,
    s12e0r,
    s12e0w,
}

#[derive(Debug, Clone, Copy)]
pub enum Dc {
    zva,
    ivac,
    isw,
    cvac,
    csw,
    cvau,
    civac,
    cisw,
}

#[derive(Debug, Clone, Copy)]
pub enum Ic {
    ialluis,
    iallu,
    ivau,
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
    At(At),
    Dc(Dc),
    Ic(Ic),
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
pub enum PrefetchWhat {
    pld,
    pli,
    pst,
}

#[derive(Debug, Clone, Copy)]
pub struct Prefetch {
    pub what: PrefetchWhat,
    pub level: u8,
    pub keep: bool,
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

impl fmt::Display for Barrier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let label = match self {
            Self::Reads(BarrierDomain::InnerShareable) => "ishld",
            Self::Reads(BarrierDomain::Nonshareable) => "nshld",
            Self::Reads(BarrierDomain::OuterShareable) => "oshld",
            Self::Reads(BarrierDomain::FullSystem) => "ld",
            Self::Writes(BarrierDomain::InnerShareable) => "ishst",
            Self::Writes(BarrierDomain::Nonshareable) => "nshst",
            Self::Writes(BarrierDomain::OuterShareable) => "oshst",
            Self::Writes(BarrierDomain::FullSystem) => "st",
            Self::All(BarrierDomain::InnerShareable) => "ish",
            Self::All(BarrierDomain::Nonshareable) => "nsh",
            Self::All(BarrierDomain::OuterShareable) => "osh",
            Self::All(BarrierDomain::FullSystem) => "sy",
        };

        f.write_str(label)
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
            0 => Ok(Self::eq),
            1 => Ok(Self::ne),
            2 => Ok(Self::hs),
            3 => Ok(Self::lo),
            4 => Ok(Self::mi),
            5 => Ok(Self::pl),
            6 => Ok(Self::vs),
            7 => Ok(Self::vc),
            8 => Ok(Self::hi),
            9 => Ok(Self::ls),
            10 => Ok(Self::ge),
            11 => Ok(Self::lt),
            12 => Ok(Self::gt),
            13 => Ok(Self::le),
            14 => Ok(Self::al),
            15 => Ok(Self::nv),
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

impl fmt::Display for SysOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::At(at) => fmt::Debug::fmt(at, f),
            Self::Dc(dc) => fmt::Debug::fmt(dc, f),
            Self::Ic(ic) => fmt::Debug::fmt(ic, f),
            Self::Tlbi(tlbi) => fmt::Debug::fmt(tlbi, f),
        }
    }
}

impl Prefetch {
    pub fn decode(rt: u8) -> Option<Self> {
        let keep = rt & 1 == 0;
        let level = ((rt >> 1) & 0x3) + 1;
        if level == 0b100 {
            return None;
        }
        let what = match (rt >> 3) & 0x3 {
            0b00 => PrefetchWhat::pld,
            0b01 => PrefetchWhat::pli,
            0b10 => PrefetchWhat::pst,
            _ => return None,
        };
        Some(Self { what, keep, level })
    }
}

impl fmt::Display for Prefetch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "{:?}l{}{}",
            self.what,
            self.level,
            if self.keep { "keep" } else { "strm" }
        ))
    }
}
