use core::fmt;

use crate::SystemReg;

/// Describes a single operand of an instruction.
#[derive(Debug, Clone, Copy)]
pub enum Operand {
    /// `Wn|wzr` operand.
    W(u8),
    /// `Xn|xzr` operand.
    X(u8),
    /// `Xn|sp` operand.
    XSp(u8),
    /// `Wn|wsp` operand.
    WSp(u8),
    /// See [crate::VectorMulti].
    VMulti(VectorMulti),
    /// See [crate::VectorSingle].
    VSingle(VectorSingle),
    /// See [crate::VectorSingle], with index.
    VSingleIndex(VectorSingle, u8),
    /// See [VectorMultiGroup].
    VMultiGroup(VectorMultiGroup),
    /// See [VectorSingleGroup].
    VSingleGroup(VectorSingleGroup),
    /// Decimal immediate `#1234`.
    DecImm(u64),
    /// Floating point immediate `#123.456`.
    #[cfg_attr(docsrs, doc(cfg(feature = "std")))]
    #[cfg(feature = "std")]
    FpImm(f64),
    /// Floating point immediate `#<float>`, used when "std" feature is not enabled.
    FpImmUnknown,
    /// Floating point zero constant `#0.0`.
    FpZero,
    /// Hexadecimal immediate `#0x1234ABCD`.
    Imm(u64),
    /// Signed hexadecimal immediate `#-0x1234`.
    Simm(i64),
    /// pc-relative immediate offset. Used together with [crate::SymbolResolver] to display
    /// program locations being referenced.
    PcRelImm(i64),
    /// pc-relative immediate offset. Resulting address calculated as:
    /// `(pc + offset) & !0xFFF`
    Adrp(i64),
    /// Xn|sp-relative offset `[Xn|sp, <index>]`. See [crate::IndexMode].
    MemXSpOff(u8, IndexMode),
    /// Repeating ones shift left `msl #16`.
    Msl(u8),
    /// Left shift `lsl #3`.
    Lsl(u8),
    /// Right shift `lsr #3`.
    Lsr(u8),
    /// Arithmetic right shift `asr #3`.
    Asr(u8),
    /// Rotate right `ror #3`.
    Ror(u8),
    /// Register extension operand. See [crate::RegExtend].
    RegExtend(RegExtend),
    /// Memory barrier operand. See [crate::Barrier].
    Barrier(Barrier),
    /// System (MSR) register operand. See [crate::SystemReg].
    Sys(SystemReg),
    /// Pair of "system" operands as used in `sys`/`sysl` instructions: `c0, c2`.
    SysC(u8, u8),
    /// `sys` instruction operand. See [crate::SysOp].
    SysOp(SysOp),
    /// `daifset` pseudo-MSR.
    Daifset,
    /// `daifclr` pseudo-MSR.
    Daifclr,
    /// `spsel` MSR.
    Spsel,
    /// Prefetch operation operand. See [crate::Prefetch].
    Prefetch(Prefetch),
    /// Conditional operand. See [crate::BranchCondition].
    Cond(BranchCondition),
}

/// Describes a part of a vector being referenced.
#[derive(Debug, Clone, Copy)]
pub enum VectorPart {
    /// Low scalar part of the vector.
    LowScalar,
    /// Low 64-bit half of the vector.
    Low64,
    /// Whole vector.
    All,
}

/// Describes a vector register reference in the form: `<vN>.<T>[<index>]` where `<T>` is `8b`,
/// `16b`, ..., `1d`, `2d`, `1q`.
#[derive(Debug, Clone, Copy)]
pub struct VectorMulti {
    /// Register index (`<vN>`).
    pub index: u8,
    /// Lane width. Can be 8, 16, 32, 64 or 128 bits.
    pub lane_width: u8,
    /// Optional lane index (`[<index>]`).
    pub lane_index: Option<u8>,
    /// Part of the vector being referenced.
    pub part: VectorPart,
}

/// Describes a vector register reference in the form: `<vN>.<T>` where `<T>` is `B`, `H`, `S`
/// or `D`.
#[derive(Debug, Clone, Copy)]
pub struct VectorSingle {
    /// Register index (`<vN>`).
    pub index: u8,
    /// Lane width. Can be 8, 16, 32 or 64 bits.
    pub lane_width: u8,
}

/// Describes a group of operands in the form: `{ <vN>.<T>, <vN + 1>.<T>, ... }` where
/// `<T>` is `8b`, `16b`, ..., `1d`, `2d`, `1q`.
#[derive(Debug, Clone, Copy)]
pub struct VectorMultiGroup {
    /// "Base" register of the group.
    pub base: VectorMulti,
    /// Length of the group.
    pub size: usize,
}

/// Describes a group of operands in the form: `{ <vN>.<T>, <vN + 1>.<T>, ... }[<index>]`, where
/// `<T>` is `B`, `H`, `S` or `D`.
#[derive(Debug, Clone, Copy)]
pub struct VectorSingleGroup {
    /// "Base" register of the group.
    pub base: VectorSingle,
    /// Length of the group.
    pub size: usize,
    /// Index of the group.
    pub index: u8,
}

/// Describes a branch condition as used in `b.<cond>` instruction, or a conditional operand as
/// used in instructions such as `ccmp`.
#[allow(missing_docs)]
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

/// Describes a register extension method.
#[allow(missing_docs)]
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

/// Describes an extended-register offset
#[derive(Debug, Clone, Copy)]
pub struct RegExtend {
    /// Method by which the register is extended.
    pub word: RegExtendWord,
    /// Amount by which the register is extended.
    pub amount: Option<u8>,
}

/// Describes an operand to the `at` instruction.
#[allow(missing_docs)]
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

/// Describes an operand to the `dc` instruction.
#[allow(missing_docs)]
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

/// Describes an operand to the `ic` instruction.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy)]
pub enum Ic {
    ialluis,
    iallu,
    ivau,
}

/// Describes an operand to the `tlbi` instruction.
#[allow(missing_docs)]
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

/// Describes a system operation as used by `sys` instruction.
#[derive(Debug, Clone, Copy)]
pub enum SysOp {
    /// `at` instruction.
    At(At),
    /// `dc` instruction.
    Dc(Dc),
    /// `ic` instruction.
    Ic(Ic),
    /// `tlbi` instruction.
    Tlbi(Tlbi),
}

/// Describes a memory domain affected by a barrier operation.
#[derive(Debug, Clone, Copy)]
pub enum BarrierDomain {
    /// Outer shareable domain.
    OuterShareable,
    /// Non-shareable domain.
    Nonshareable,
    /// Inner shareable domain.
    InnerShareable,
    /// Full system.
    FullSystem,
}

/// Describes a memory/instruction synchronization operation.
#[derive(Debug, Clone, Copy)]
pub enum Barrier {
    /// Barrier on reads for a given domain.
    Reads(BarrierDomain),
    /// Barrier on writes for a given domain.
    Writes(BarrierDomain),
    /// Barrier on reads/writes for a given domain.
    All(BarrierDomain),
}

/// Describes type of a prefetch operation.
#[derive(Debug, Clone, Copy)]
pub enum PrefetchWhat {
    /// Prefetch for load.
    pld,
    /// Preload instructions.
    pli,
    /// Prefetch for store.
    pst,
}

/// Describes a prefetch operation.
#[derive(Debug, Clone, Copy)]
pub struct Prefetch {
    /// Describes prefetch type.
    pub what: PrefetchWhat,
    /// Describes target cache level. 1 = L1, 2 = L2, 3 = L3.
    pub level: u8,
    /// * If `true`, defines a `KEEP` retention policy, allocating in the cache normally.
    /// * If `false`, defines a `STRM` retention policy for streaming or non-temporal prefetch for
    /// data that is used only once.
    pub keep: bool,
}

/// Index mode used with Xn|sp-relative addressing.
#[derive(Debug, Clone, Copy)]
pub enum IndexMode {
    /// Pre-indexed mode: `[Xn|sp, #<imm>]!`.
    PreIndex(i64),
    /// Unsigned immediate offset: `[Xn|sp, #<pimm>]`.
    Unsigned(u64),
    /// Signed immediate offset: `[Xn|sp, #<simm>]`.
    Signed(i64),
    /// Register offset: `[Xn|sp, Xm]`.
    X(u8),
    /// Offset by `Wm`, extended as described in [RegExtend].
    WExt(u8, RegExtend),
    /// Offset by `Xm`, extended as described in [RegExtend].
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
            pub(crate) fn $word(amount: Option<u8>) -> Self {
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

    pub(crate) fn is_displayed(&self) -> bool {
        self.word != RegExtendWord::lsl || self.amount.is_some()
    }

    pub(crate) fn decode_index(Rm: u8, option: u8, amount: Option<u8>) -> Option<IndexMode> {
        let extend = Self::decode4(option, amount)?;
        let index = match option & 1 != 0 {
            false => IndexMode::WExt(Rm, extend),
            true => IndexMode::XExt(Rm, extend),
        };
        Some(index)
    }

    pub(crate) fn decode4(option: u8, amount: Option<u8>) -> Option<Self> {
        match option {
            0b010 => Some(Self::uxtw(amount)),
            0b011 => Some(Self::lsl(amount)),
            0b110 => Some(Self::sxtw(amount)),
            0b111 => Some(Self::sxtx(amount)),
            _ => None,
        }
    }

    pub(crate) fn decode(option: u8, amount: Option<u8>) -> Self {
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
    pub(crate) fn decode(rt: u8) -> Option<Self> {
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
    pub(crate) fn b(x: u8) -> Self {
        Self {
            index: x,
            lane_width: 8,
            lane_index: None,
            part: VectorPart::LowScalar,
        }
    }

    pub(crate) fn h(x: u8) -> Self {
        Self {
            index: x,
            lane_width: 16,
            lane_index: None,
            part: VectorPart::LowScalar,
        }
    }

    pub(crate) fn s(x: u8) -> Self {
        Self {
            index: x,
            lane_width: 32,
            lane_index: None,
            part: VectorPart::LowScalar,
        }
    }

    pub(crate) fn d(x: u8) -> Self {
        Self {
            index: x,
            lane_width: 64,
            lane_index: None,
            part: VectorPart::LowScalar,
        }
    }

    pub(crate) fn q(x: u8) -> Self {
        Self {
            index: x,
            lane_width: 128,
            lane_index: None,
            part: VectorPart::LowScalar,
        }
    }

    pub(crate) fn v8b(x: u8, i: Option<u8>) -> Self {
        Self {
            index: x,
            lane_width: 8, // or 1
            lane_index: i,
            part: VectorPart::Low64,
        }
    }

    pub(crate) fn v16b(x: u8, i: Option<u8>) -> Self {
        Self {
            index: x,
            lane_width: 8, // or 1
            lane_index: i,
            part: VectorPart::All,
        }
    }

    pub(crate) fn v4h(x: u8, i: Option<u8>) -> Self {
        Self {
            index: x,
            lane_width: 16,
            lane_index: i,
            part: VectorPart::Low64,
        }
    }

    pub(crate) fn v8h(x: u8, i: Option<u8>) -> Self {
        Self {
            index: x,
            lane_width: 16,
            lane_index: i,
            part: VectorPart::All,
        }
    }

    pub(crate) fn v2s(x: u8, i: Option<u8>) -> Self {
        Self {
            index: x,
            lane_width: 32,
            lane_index: i,
            part: VectorPart::Low64,
        }
    }

    pub(crate) fn v4s(x: u8, i: Option<u8>) -> Self {
        Self {
            index: x,
            lane_width: 32,
            lane_index: i,
            part: VectorPart::All,
        }
    }

    pub(crate) fn v1d(x: u8, i: Option<u8>) -> Self {
        Self {
            index: x,
            lane_width: 64,
            lane_index: i,
            part: VectorPart::Low64,
        }
    }

    pub(crate) fn v2d(x: u8, i: Option<u8>) -> Self {
        Self {
            index: x,
            lane_width: 64,
            lane_index: i,
            part: VectorPart::All,
        }
    }

    pub(crate) fn with_index(self, index: u8) -> Self {
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
    pub(crate) fn b(index: u8) -> Self {
        Self {
            index,
            lane_width: 8,
        }
    }
    pub(crate) fn h(index: u8) -> Self {
        Self {
            index,
            lane_width: 16,
        }
    }
    pub(crate) fn s(index: u8) -> Self {
        Self {
            index,
            lane_width: 32,
        }
    }
    pub(crate) fn d(index: u8) -> Self {
        Self {
            index,
            lane_width: 64,
        }
    }

    pub(crate) fn with_index(self, index: u8) -> Self {
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

        assert_eq!(format!("{}", VectorMulti::v8b(1, Some(0))), "v1.8b[0]");
    }
}
