use core::fmt;

use crate::BranchCondition;

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
    CondB(BranchCondition),
}

impl fmt::Display for Mnemonic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CondB(br) => f.write_fmt(format_args!("b.{:?}", br)),
            Self::yield_ => f.write_str("yield"),
            _ => fmt::Debug::fmt(self, f),
        }
    }
}
