use core::fmt;

use crate::SystemReg;

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    W(u8),
    X(u8),
    XSp(u8),
    WSp(u8),

    // vN.Xt
    VMulti(VectorMulti),
    // vN.t
    VSingle(VectorSingle),
    // { vN.Xt, vM.Yt ... }
    VMultiGroup(VectorMultiGroup),
    // { vN.t, vM.t, ... }[i]
    VSingleGroup(VectorSingleGroup),

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
pub enum VectorPart {
    LowScalar,
    Low64,
    All,
}

#[derive(Debug, Clone, Copy)]
pub struct VectorMulti {
    pub index: u8,
    pub lane_width: u8,
    pub lane_index: Option<u8>,
    pub part: VectorPart,
}

#[derive(Debug, Clone, Copy)]
pub struct VectorSingle {
    pub index: u8,
    pub lane_width: u8,
}

#[derive(Debug, Clone, Copy)]
pub struct VectorMultiGroup {
    pub base: VectorMulti,
    pub size: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct VectorSingleGroup {
    pub base: VectorSingle,
    pub size: usize,
    pub index: u8,
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
    pub amount: Option<u8>,
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

// Formatting helper
struct MaybeIndex(Option<u8>);

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
            pub fn $word(amount: Option<u8>) -> Self {
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
        self.word != RegExtendWord::lsl || !self.amount.is_none()
    }

    pub fn decode_index(Rm: u8, option: u8, amount: Option<u8>) -> Option<IndexMode> {
        let extend = Self::decode4(option, amount)?;
        let index = match option & 1 != 0 {
            false => IndexMode::WExt(Rm, extend),
            true => IndexMode::XExt(Rm, extend),
        };
        Some(index)
    }

    pub fn decode4(option: u8, amount: Option<u8>) -> Option<Self> {
        match option {
            0b010 => Some(Self::uxtw(amount)),
            0b011 => Some(Self::lsl(amount)),
            0b110 => Some(Self::sxtw(amount)),
            0b111 => Some(Self::sxtx(amount)),
            _ => None,
        }
    }

    pub fn decode(option: u8, amount: Option<u8>) -> Self {
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
        if let Some(amount) = self.amount {
            f.write_fmt(format_args!(" #{}", amount))
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

impl VectorMulti {
    pub fn b(x: u8) -> Self {
        Self {
            index: x,
            lane_width: 8,
            lane_index: None,
            part: VectorPart::LowScalar,
        }
    }

    pub fn h(x: u8) -> Self {
        Self {
            index: x,
            lane_width: 16,
            lane_index: None,
            part: VectorPart::LowScalar,
        }
    }

    pub fn s(x: u8) -> Self {
        Self {
            index: x,
            lane_width: 32,
            lane_index: None,
            part: VectorPart::LowScalar,
        }
    }

    pub fn d(x: u8) -> Self {
        Self {
            index: x,
            lane_width: 64,
            lane_index: None,
            part: VectorPart::LowScalar,
        }
    }

    pub fn q(x: u8) -> Self {
        Self {
            index: x,
            lane_width: 128,
            lane_index: None,
            part: VectorPart::LowScalar,
        }
    }

    pub fn v8b(x: u8, i: Option<u8>) -> Self {
        Self {
            index: x,
            lane_width: 8, // or 1
            lane_index: i,
            part: VectorPart::Low64,
        }
    }

    pub fn v16b(x: u8, i: Option<u8>) -> Self {
        Self {
            index: x,
            lane_width: 8, // or 1
            lane_index: i,
            part: VectorPart::All,
        }
    }

    pub fn v4h(x: u8, i: Option<u8>) -> Self {
        Self {
            index: x,
            lane_width: 16,
            lane_index: i,
            part: VectorPart::Low64,
        }
    }

    pub fn v8h(x: u8, i: Option<u8>) -> Self {
        Self {
            index: x,
            lane_width: 16,
            lane_index: i,
            part: VectorPart::All,
        }
    }

    pub fn v2s(x: u8, i: Option<u8>) -> Self {
        Self {
            index: x,
            lane_width: 32,
            lane_index: i,
            part: VectorPart::Low64,
        }
    }

    pub fn v4s(x: u8, i: Option<u8>) -> Self {
        Self {
            index: x,
            lane_width: 32,
            lane_index: i,
            part: VectorPart::All,
        }
    }

    pub fn v1d(x: u8, i: Option<u8>) -> Self {
        Self {
            index: x,
            lane_width: 64,
            lane_index: i,
            part: VectorPart::Low64,
        }
    }

    pub fn v2d(x: u8, i: Option<u8>) -> Self {
        Self {
            index: x,
            lane_width: 64,
            lane_index: i,
            part: VectorPart::All,
        }
    }

    pub fn v1q(x: u8, i: Option<u8>) -> Self {
        Self {
            index: x,
            lane_width: 128,
            lane_index: i,
            part: VectorPart::All,
        }
    }

    pub fn with_index(self, index: u8) -> Self {
        Self { index, ..self }
    }
}

impl fmt::Display for VectorMulti {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (self.part, self.lane_width, self.lane_index) {
            (VectorPart::LowScalar, 8, _) => write!(f, "b{}", self.index),
            (VectorPart::LowScalar, 16, _) => write!(f, "h{}", self.index),
            (VectorPart::LowScalar, 32, _) => write!(f, "s{}", self.index),
            (VectorPart::LowScalar, 64, _) => write!(f, "d{}", self.index),
            (VectorPart::LowScalar, 128, _) => write!(f, "q{}", self.index),

            (VectorPart::Low64, 8, i) => write!(f, "v{}.8b{}", self.index, MaybeIndex(i)),
            (VectorPart::Low64, 16, i) => write!(f, "v{}.4h{}", self.index, MaybeIndex(i)),
            (VectorPart::Low64, 32, i) => write!(f, "v{}.2s{}", self.index, MaybeIndex(i)),
            (VectorPart::Low64, 64, i) => write!(f, "v{}.1d{}", self.index, MaybeIndex(i)),

            (VectorPart::All, 8, i) => write!(f, "v{}.16b{}", self.index, MaybeIndex(i)),
            (VectorPart::All, 16, i) => write!(f, "v{}.8h{}", self.index, MaybeIndex(i)),
            (VectorPart::All, 32, i) => write!(f, "v{}.4s{}", self.index, MaybeIndex(i)),
            (VectorPart::All, 64, i) => write!(f, "v{}.2d{}", self.index, MaybeIndex(i)),
            (VectorPart::All, 128, i) => write!(f, "v{}.1q{}", self.index, MaybeIndex(i)),

            _ => unimplemented!(),
        }
    }
}

impl VectorSingle {
    pub fn b(index: u8) -> Self {
        Self {
            index,
            lane_width: 8,
        }
    }
    pub fn h(index: u8) -> Self {
        Self {
            index,
            lane_width: 16,
        }
    }
    pub fn s(index: u8) -> Self {
        Self {
            index,
            lane_width: 32,
        }
    }
    pub fn d(index: u8) -> Self {
        Self {
            index,
            lane_width: 64,
        }
    }

    pub fn with_index(self, index: u8) -> Self {
        Self { index, ..self }
    }
}

impl fmt::Display for VectorSingle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let letter = match self.lane_width {
            8 => 'b',
            16 => 'h',
            32 => 's',
            64 => 'd',
            128 => 'q',
            _ => unimplemented!(),
        };

        write!(f, "v{}.{}", self.index, letter)
    }
}

impl fmt::Display for VectorMultiGroup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        debug_assert!(self.size != 0 && self.size <= 4);
        f.write_str("{ ")?;
        for i in 0..self.size {
            if i != 0 {
                f.write_str(", ")?;
            }
            let reg = self.base.with_index((self.base.index + i as u8) % 32);
            fmt::Display::fmt(&reg, f)?;
        }
        f.write_str(" }")
    }
}

impl fmt::Display for VectorSingleGroup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        debug_assert!(self.size != 0 && self.size <= 4);
        f.write_str("{ ")?;
        for i in 0..self.size {
            if i != 0 {
                f.write_str(", ")?;
            }
            let reg = self.base.with_index((self.base.index + i as u8) % 32);
            fmt::Display::fmt(&reg, f)?;
        }
        write!(f, " }}[{}]", self.index)
    }
}

impl fmt::Display for MaybeIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            None => Ok(()),
            Some(i) => write!(f, "[{}]", i),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::operand::VectorMulti;

    #[test]
    fn vector_multi_display() {
        assert_eq!(format!("{}", VectorMulti::b(1)), "b1");
        assert_eq!(format!("{}", VectorMulti::h(3)), "h3");
        assert_eq!(format!("{}", VectorMulti::s(17)), "s17");
        assert_eq!(format!("{}", VectorMulti::d(21)), "d21");
        assert_eq!(format!("{}", VectorMulti::q(31)), "q31");

        assert_eq!(format!("{}", VectorMulti::v8b(1, None)), "v1.8b");
        assert_eq!(format!("{}", VectorMulti::v8b(1, Some(1))), "v1.8b[1]");
        assert_eq!(format!("{}", VectorMulti::v4h(3, None)), "v3.4h");
        assert_eq!(format!("{}", VectorMulti::v4h(3, Some(2))), "v3.4h[2]");
        assert_eq!(format!("{}", VectorMulti::v2s(17, None)), "v17.2s");
        assert_eq!(format!("{}", VectorMulti::v2s(17, Some(1))), "v17.2s[1]");
        assert_eq!(format!("{}", VectorMulti::v1d(21, None)), "v21.1d");
        assert_eq!(format!("{}", VectorMulti::v1d(21, Some(1))), "v21.1d[1]");

        assert_eq!(format!("{}", VectorMulti::v16b(0, None)), "v0.16b");
        assert_eq!(format!("{}", VectorMulti::v16b(3, Some(3))), "v3.16b[3]");
        assert_eq!(format!("{}", VectorMulti::v8h(7, None)), "v7.8h");
        assert_eq!(format!("{}", VectorMulti::v8h(7, Some(3))), "v7.8h[3]");
        assert_eq!(format!("{}", VectorMulti::v4s(17, None)), "v17.4s");
        assert_eq!(format!("{}", VectorMulti::v4s(17, Some(1))), "v17.4s[1]");
        assert_eq!(format!("{}", VectorMulti::v2d(21, None)), "v21.2d");
        assert_eq!(format!("{}", VectorMulti::v2d(21, Some(1))), "v21.2d[1]");
        assert_eq!(format!("{}", VectorMulti::v1q(31, None)), "v31.1q");

        assert_eq!(format!("{}", VectorMulti::v8b(1, Some(0))), "v1.8b[0]");
    }
}
