use core::fmt;

use crate::BranchCondition;

/// Describes a single instruction's mnemonic, as used in the assembly language.
#[allow(missing_docs)]
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
    at,
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
    cinv,
    clrex,
    cls,
    clz,
    cmn,
    cmp,
    cneg,
    crc32b,
    crc32cb,
    crc32ch,
    crc32cw,
    crc32cx,
    crc32h,
    crc32w,
    crc32x,
    csel,
    cset,
    csetm,
    csinc,
    csinv,
    csneg,
    dc,
    dcps1,
    dcps2,
    dcps3,
    dmb,
    drps,
    dsb,
    eon,
    eor,
    eret,
    extr,
    hint,
    hlt,
    hvc,
    ic,
    isb,
    ld1,
    ld1r,
    ld2,
    ld2r,
    ld3,
    ld3r,
    ld4,
    ld4r,
    ldar,
    ldarb,
    ldarh,
    ldaxp,
    ldaxr,
    ldaxrb,
    ldaxrh,
    ldnp,
    ldp,
    ldr,
    ldrb,
    ldrh,
    ldrsb,
    ldrsh,
    ldrsw,
    ldtr,
    ldtrb,
    ldtrh,
    ldtrsb,
    ldtrsh,
    ldtrsw,
    ldur,
    ldurb,
    ldurh,
    ldursb,
    ldursh,
    ldursw,
    ldxp,
    ldxr,
    ldxrb,
    ldxrh,
    lsl,
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
    neg,
    negs,
    ngc,
    ngcs,
    nop,
    orn,
    orr,
    prfm,
    prfum,
    rbit,
    ret,
    rev,
    rev16,
    rev32,
    ror,
    sbc,
    sbcs,
    sbfiz,
    sbfm,
    sbfx,
    sev,
    sevl,
    sdiv,
    smaddl,
    smc,
    smnegl,
    smsubl,
    smulh,
    smull,
    st1,
    st2,
    st3,
    st4,
    stlr,
    stlrb,
    stlrh,
    stlxp,
    stlxr,
    stlxrb,
    stlxrh,
    stnp,
    stp,
    str,
    strb,
    strh,
    sttr,
    sttrb,
    sttrh,
    stur,
    sturb,
    sturh,
    stxp,
    stxr,
    stxrb,
    stxrh,
    sub,
    subs,
    svc,
    sxtb,
    sxth,
    sxtw,
    sys,
    sysl,
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
    umnegl,
    umsubl,
    umull,
    umulh,
    uxtb,
    uxth,
    wfe,
    wfi,
    yield_,
    /// SIMD instruction mnemonic. See [SimdMnemonic].
    #[cfg(feature = "simd")]
    Simd(SimdMnemonic),
    /// Dummy mnemonic `.simd` used to print opcodes when the "simd" feature of the crate is not
    /// enabled.
    #[cfg(not(feature = "simd"))]
    Simd(u32),
    /// Conditional branch in the form: `b.<cond>`.
    CondB(BranchCondition),
}

/// Describes a SIMD instruction mnemonic.
#[cfg(feature = "simd")]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy)]
pub enum SimdMnemonic {
    fmadd,
    fmsub,
    fnmadd,
    fnmsub,
    fcsel,
    fccmp,
    fccmpe,
    fcmp,
    fcmpe,
    fmov,
    movi,
    mvni,
    abs,
    add,
    addhn,
    addhn2,
    addp,
    addv,
    aesd,
    aese,
    aesimc,
    aesmc,
    and,
    bic,
    bif,
    bit,
    bsl,
    cls,
    clz,
    cmeq,
    cmge,
    cmgt,
    cmhi,
    cmhs,
    cmle,
    cmlt,
    cmtst,
    cnt,
    dup,
    eor,
    ext,
    fabd,
    fabs,
    facge,
    facgt,
    fadd,
    faddp,
    fcmeq,
    fcmge,
    fcmgt,
    fcmle,
    fcmlt,
    fcvtas,
    fcvtau,
    fcvtl,
    fcvtl2,
    fcvtms,
    fcvtmu,
    fcvtn,
    fcvtn2,
    fcvtns,
    fcvtnu,
    fcvtps,
    fcvtpu,
    fcvtxn,
    fcvtxn2,
    fcvtzs,
    fcvtzu,
    fdiv,
    fmax,
    fmaxnm,
    fmaxnmp,
    fmaxnmv,
    fmaxp,
    fmaxv,
    fmin,
    fminnm,
    fnmul,
    fminnmp,
    fminnmv,
    fminp,
    fminv,
    fmla,
    fmls,
    fmul,
    fmulx,
    fneg,
    frecpe,
    frecps,
    frecpx,
    frinta,
    frinti,
    frintm,
    frintn,
    frintp,
    frintx,
    frintz,
    frsqrte,
    frsqrts,
    fsqrt,
    fcvt,
    fsub,
    mla,
    mls,
    mov,
    mul,
    mvn,
    neg,
    not,
    orn,
    orr,
    pmul,
    pmull,
    pmull2,
    raddhn,
    raddhn2,
    rbit,
    rev16,
    rev32,
    rev64,
    rsubhn,
    rsubhn2,
    saba,
    sabal,
    sabal2,
    sabd,
    sabdl,
    sabdl2,
    sadalp,
    saddl,
    saddl2,
    saddlp,
    saddlv,
    saddw,
    saddw2,
    scvtf,
    sha1c,
    sha1h,
    sha1m,
    sha1p,
    sha1su0,
    sha1su1,
    sha256h,
    sha256h2,
    sha256su0,
    sha256su1,
    shadd,
    shl,
    shll,
    shsub,
    sli,
    smax,
    smaxp,
    smaxv,
    smin,
    sminp,
    sminv,
    smlal,
    smlal2,
    smlsl,
    smlsl2,
    smov,
    smull,
    smull2,
    sqabs,
    sqadd,
    sqdmlal,
    sqdmlal2,
    sqdmlsl,
    sqdmlsl2,
    sqdmulh,
    sqdmulh2,
    sqdmull,
    sqdmull2,
    sqneg,
    sqrdmulh,
    sqrshl,
    sqrshrn,
    sshll,
    sshll2,
    sqrshrn2,
    sqrshrun,
    sqrshrun2,
    sqshl,
    shrn,
    rshrn,
    rshrn2,
    shrn2,
    sqshlu,
    sqshrn,
    sqshrn2,
    sqshrun,
    sqshrun2,
    sqsub,
    sqxtn,
    sqxtn2,
    sqxtun,
    sqxtun2,
    srhadd,
    sri,
    srshl,
    srshr,
    srsra,
    sshl,
    sshr,
    ssra,
    ssubl,
    ssubl2,
    ssubw,
    ssubw2,
    sub,
    subhn,
    subhn2,
    suqadd,
    tbl,
    tbx,
    trn1,
    trn2,
    uaba,
    uabal,
    uabal2,
    uabd,
    uabdl,
    uabdl2,
    uadalp,
    uaddl,
    uaddl2,
    uaddlp,
    uaddlv,
    uaddw,
    uaddw2,
    ucvtf,
    uhadd,
    uhsub,
    umax,
    umaxp,
    umaxv,
    umin,
    uminp,
    uminv,
    umlal,
    umlal2,
    umlsl,
    umlsl2,
    umov,
    umull,
    umull2,
    uqadd,
    uqrshl,
    uqrshrn,
    uqrshrn2,
    ushll,
    ushll2,
    uqshl,
    uqshrn,
    uqshrn2,
    uqsub,
    uqxtn,
    uqxtn2,
    urecpe,
    urhadd,
    urshl,
    urshr,
    ursqrte,
    ursra,
    ushl,
    ushr,
    usqadd,
    usra,
    usubl,
    usubl2,
    usubw,
    usubw2,
    uzp1,
    uzp2,
    xtn,
    xtn2,
    zip1,
    zip2,
}

impl fmt::Display for Mnemonic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CondB(br) => write!(f, "b.{:?}", br),
            #[cfg(feature = "simd")]
            Self::Simd(simd) => fmt::Debug::fmt(simd, f),
            #[cfg(not(feature = "simd"))]
            Self::Simd(simd) => write!(f, ".simd {:#010x}", simd),
            Self::yield_ => f.write_str("yield"),
            _ => fmt::Debug::fmt(self, f),
        }
    }
}
