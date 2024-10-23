// loads and stores, excluding SIMD
.section .text
load_store_excl:
    stxrb w1, w2, [x3]
    stlxrb w1, w2, [x3]
    stlrb w2, [x3]

    ldxrb w1, [x3]
    ldaxrb w1, [x3]
    ldarb w1, [x3]

    stxrh w1, w2, [x3]
    stlxrh w1, w2, [x3]
    stlrh w2, [x3]

    ldxrh w1, [x3]
    ldaxrh w1, [x3]
    ldarh w1, [x3]

    stxr w1, w2, [x3]
    stlxr w1, w2, [x3]
    stlr w2, [x3]

    stxr w1, x2, [x3]
    stlxr w1, x2, [x3]
    stlr x2, [x3]

    ldxr w1, [x3]
    ldaxr w1, [x3]
    ldar w1, [x3]

    ldxr x1, [x3]
    ldaxr x1, [x3]
    ldar x1, [x3]

    stxp w1, w2, w3, [x4]
    stlxp w1, w2, w3, [x4]

    ldxp w1, w2, [x4]
    ldaxp w1, w2, [x4]

    stxp w1, x2, x3, [x4]
    stlxp w1, x2, x3, [x4]

    ldxp x1, x2, [x4]
    ldaxp x1, x2, [x4]
.size load_store_excl, . - load_store_excl

load_literal:
    ldr w1, =0x12345678
    ldr x2, =0x1111222233334444
    ldrsw x3, =0x1234
1:
    prfm pldl1keep, load_store_excl
    prfm pstl2strm, 1b
.size load_literal, . - load_literal

load_store_pair:
    stnp w1, w2, [x3, #4]
    stnp x1, x2, [x3, #8]
    ldnp w1, w2, [x3, #12]
    ldnp x1, x2, [x3, #24]

    stp w1, w2, [x3], #4
    stp x1, x2, [x3], #8
    ldp w1, w2, [x3], #12
    ldp x1, x2, [x3], #16

    stp w1, w2, [x3, #4]
    stp x1, x2, [x3, #8]
    ldp w1, w2, [x3, #12]
    ldp x1, x2, [x3, #16]

    stp w1, w2, [x3, #4]!
    stp x1, x2, [x3, #8]!
    ldp w1, w2, [x3, #12]!
    ldp x1, x2, [x3, #16]!
.size load_store_pair, . - load_store_pair

load_store_single:
    sturb w1, [x3, #3]
    strb w1, [x3, #3]
    strb w1, [x3, #3]!
    strb w1, [x3], #3

    ldurb w1, [x3, #7]
    ldrb w1, [x3, #7]
    ldrb w1, [x3, #7]!
    ldrb w1, [x3], #3

    ldursb x1, [x3, #13]
    ldrsb x1, [x3, #13]
    ldrsb x1, [x3, #13]!
    ldrsb x1, [x3], #13

    ldursb w1, [x3, #17]
    ldrsb w1, [x3, #17]
    ldrsb w1, [x3, #17]!
    ldrsb w1, [x3], #17

    sturb w1, [x3, #-3]
    strb w1, [x3, #-3]
    strb w1, [x3, #-3]!
    strb w1, [x3], #-3

    ldurb w1, [x3, #-7]
    ldrb w1, [x3, #4]
    ldrb w1, [x3, #-7]!
    ldrb w1, [x3], #-7

    ldursb x1, [x3, #-13]
    ldrsb x1, [x3, #13]
    ldrsb x1, [x3, #-13]!
    ldrsb x1, [x3], #-13

    ldursb w1, [x3, #-17]
    ldrsb w1, [x3, #13]
    ldrsb w1, [x3, #-13]!
    ldrsb w1, [x3], #-13

    sturh w1, [x3, #4]
    strh w1, [x3, #4]
    strh w1, [x3, #4]!
    strh w1, [x3], #4

    ldurh w1, [x3, #5]
    ldrh w1, [x3, #6]
    ldrh w1, [x3, #6]!
    ldrh w1, [x3], #6

    ldursh x1, [x3, #7]
    ldrsh x1, [x3, #6]
    ldrsh x1, [x3, #7]!
    ldrsh x1, [x3], #7

    ldursh w1, [x3, #3]
    ldrsh w1, [x3, #4]
    ldrsh w1, [x3, #3]!
    ldrsh w1, [x3], #3

    stur w1, [x3, #7]
    str w1, [x3, #8]
    str w1, [x3, #8]!
    str w1, [x3], #8

    stur x1, [x3, #3]
    str x1, [x3, #8]
    str x1, [x3, #8]!
    str x1, [x3], #8

    ldursw x1, [x3, #7]
    ldrsw x1, [x3, #8]
    ldrsw x1, [x3, #8]!
    ldrsw x1, [x3], #8

    prfum plil3keep, [x3, #-7]
.size load_store_single, . - load_store_single

load_store_reg_off_extend:
    strb w1, [x2, w3, sxtw]
    strb w1, [x2, w3, uxtw]
    strb w1, [x2, x3, sxtx]
    ldrb w1, [x2, w3, sxtw]
    ldrb w1, [x2, w3, uxtw]
    ldrb w1, [x2, x3, sxtx]

    ldrsb w1, [x2, w3, sxtw]
    ldrsb w1, [x2, w3, uxtw]
    ldrsb w1, [x2, x3, sxtx]
    ldrsb x1, [x2, w3, sxtw]
    ldrsb x1, [x2, w3, uxtw]
    ldrsb x1, [x2, x3, sxtx]

    strh w1, [x2, w3, sxtw]
    strh w1, [x2, w3, uxtw]
    strh w1, [x2, x3, sxtx]
    ldrh w1, [x2, w3, sxtw]
    ldrh w1, [x2, w3, uxtw]
    ldrh w1, [x2, x3, sxtx]

    ldrsh w1, [x2, w3, sxtw]
    ldrsh w1, [x2, w3, uxtw]
    ldrsh w1, [x2, x3, sxtx]
    ldrsh x1, [x2, w3, sxtw]
    ldrsh x1, [x2, w3, uxtw]
    ldrsh x1, [x2, x3, sxtx]

    ldrsw x1, [x2, w3, sxtw]
    ldrsw x1, [x2, w3, uxtw]
    ldrsw x1, [x2, x3, sxtx]

    str w1, [x2, w3, sxtw]
    str w1, [x2, w3, uxtw]
    str w1, [x2, x3, sxtx]
    str x1, [x2, w3, sxtw]
    str x1, [x2, w3, uxtw]
    str x1, [x2, x3, sxtx]

    ldr w1, [x2, w3, sxtw]
    ldr w1, [x2, w3, uxtw]
    ldr w1, [x2, x3, sxtx]
    ldr x1, [x2, w3, sxtw]
    ldr x1, [x2, w3, uxtw]
    ldr x1, [x2, x3, sxtx]

    str w1, [x2, w3, sxtw #2]

    prfm pldl1keep, [x2, w3, sxtw]
    prfm pldl1keep, [x2, w3, uxtw]
    prfm pldl1keep, [x2, x3, sxtx]
.size load_store_reg_off_extend, . - load_store_reg_off_extend

load_store_reg_off_shift:
    strh w1, [x2, x3, lsl #1]
    ldrh w1, [x2, x3, lsl #1]

    ldrsh w1, [x2, x3, lsl #1]
    ldrsh x1, [x2, x3, lsl #1]

    ldrsw x1, [x2, x3, lsl #2]

    str w1, [x2, x3, lsl #2]
    str x1, [x2, x3, lsl #3]
    ldr w1, [x2, x3, lsl #2]
    ldr x1, [x2, x3, lsl #3]

    prfm pldl1keep, [x2, x3, lsl #3]
.size load_store_reg_off_shift, . - load_store_reg_off_shift

load_store_reg_off:
    strb w1, [x2, x3]
    ldrb w1, [x2, x3]

    ldrsb w1, [x2, x3]
    ldrsb x1, [x2, x3]

    strh w1, [x2, x3]
    ldrh w1, [x2, x3]

    ldrsh w1, [x2, x3]
    ldrsh x1, [x2, x3]

    ldrsw x1, [x2, x3]

    str w1, [x2, x3]
    str x1, [x2, x3]
    ldr w1, [x2, x3]
    ldr x1, [x2, x3]

    prfm pstl3strm, [x2, x3]
.size load_store_reg_off, . - load_store_reg_off

load_store_reg_uimm:
    strb w1, [x2, #4]
    ldrb w1, [x2, #4]

    ldrsb w1, [x2, #7]
    ldrsb x1, [x2, #7]

    strh w1, [x2, #6]
    ldrh w1, [x2, #6]

    ldrsh w1, [x2, #6]
    ldrsh x1, [x2, #6]

    ldrsw x1, [x2, #8]

    str w1, [x2, #12]
    str x1, [x2, #16]
    ldr w1, [x2, #12]
    ldr x1, [x2, #16]
.size load_store_reg_uimm, . - load_store_reg_uimm

load_store_unpriv:
    sttrb w1, [x2, #-2]
    ldtrb w1, [x2, #3]

    ldtrsb w1, [x2, #-3]
    ldtrsb x1, [x2, #3]

    sttrh w1, [x2, #2]
    ldtrh w1, [x2, #4]

    ldtrsh w1, [x2, #4]
    ldtrsh x1, [x2, #6]

    ldtrsw x1, [x2, #8]

    sttr w1, [x2, #4]
    sttr x1, [x2, #8]
    ldtr w1, [x2, #12]
    ldtr x1, [x2, #16]
.size load_store_unpriv, . - load_store_unpriv
