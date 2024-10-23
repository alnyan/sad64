.section .text
imm_pcrel_addressing:
    adr x3, label1
    adrp x3, label1
.size imm_pcrel_addressing, . - imm_pcrel_addressing

add_sub_immediate:
    add w1, w2, #0x123
    sub w1, w2, #0x123
    add x1, x2, #0x123
    sub x1, x2, #0x123

    # to/from sp/wsp
    add wsp, w2, #0x123
    add w2, wsp, #0x123
    add sp, x3, #0x321
    add x3, sp, #0x321

    # shifted
    add w1, w2, #0x123, lsl #12
    add x2, x3, #0x123, lsl #12
.size add_sub_immediate, . - add_sub_immediate

adds_subs_immediate:
    adds w2, w3, #0x321
    subs w2, w3, #0x321
    adds x2, x3, #0x321
    subs x2, x3, #0x321

    # to/from sp/wsp
    adds w2, wsp, #0x123
    subs w2, wsp, #0x123
    adds x3, sp, #0x321
    subs x3, sp, #0x321
.size adds_subs_immediate, . - adds_subs_immediate

add_sub_immediate_aliases:
    # aliases
    # add
    # -> mov sp, r1
    add wsp, w1, #0
    mov wsp, w1
    add sp, x1, #0
    mov sp, x1
    # -> mov r1, sp
    add w1, wsp, #0
    mov w1, wsp
    add x1, sp, #0
    mov x1, sp

    # -> cmn r1, imm
    adds wzr, w1, #0x123
    cmn w1, #0x123
    adds xzr, x2, #0x321
    cmn x2, #0x321

    # -> cmp r1, imm
    subs wzr, w1, #0x123
    cmp w1, #0x123
    subs xzr, x2, #0x321
    cmp x2, #0x321
.size add_sub_immediate_aliases, . - add_sub_immediate_aliases

logical_imm:
    and w1, w2, #0x3F
    and x2, x3, #0x1F

    orr w1, w2, #0x3F
    orr x2, x3, #0x1F

    eor w1, w2, #0x3F
    eor x2, x3, #0x1F

    ands w1, w2, #0x3F
    ands x2, x3, #0x1F
.size logical_imm, . - logical_imm

logical_imm_aliases:
    orr wsp, wzr, #0x10101010
    mov wsp, #0x10101010
    orr sp, xzr, #0xFFFFFFFF
    mov sp, #0xFFFFFFFF

    # ands -> tst
    ands wzr, w2, #0x3F
    tst w2, #0x3F
    ands xzr, x3, #0x1F
    tst x3, #0x1F
.size logical_imm_aliases, . - logical_imm_aliases

move_wide_immediate:
    movn w1, #0, lsl #16
    movn x1, #0, lsl #16
    movn x2, #0, lsl #32
    movn x3, #0, lsl #48
    movn w1, #0xFFFF
    movn w1, #0xFFFF, lsl #16

    movz w1, #0, lsl #16
    movz x1, #0, lsl #16
    movz x2, #0, lsl #32
    movz x3, #0, lsl #48

    movk w1, #0xFF
    movk x1, #0xFF
    movk w1, #0x1234, lsl #16
    movk x1, #0x1234, lsl #32
.size move_wide_immediate, . - move_wide_immediate

move_wide_immediate_aliases:
    # movn -> mov inverted wide immediate
    movn w1, #0x1234, lsl #16
    mov w1, #-0x12340001
    movn x1, #0x4321, lsl #32
    mov x1, #-0x432100000001
    movn w1, #0x1234
    mov w1, #-0x1235
    movn x1, #0x1234
    mov x1, #-0x1235

    # movz -> mov wide immediate
    movz w1, #0x1234
    mov w1, #0x1234
    movz x1, #0x4321
    mov x1, #0x4321
    movz w1, #0x1234, lsl #16
    mov w1, #0x12340000
    movz x1, #0xFFFF, lsl #48
    mov x1, #-0x1000000000000
.size move_wide_immediate_aliases, . - move_wide_immediate_aliases

sbfm:
    # sbfm -> asr
    sbfm w1, w2, #0x4, #0x1F
    asr w1, w2, #4
    sbfm x1, x2, #0x13, #0x3F
    asr x1, x2, #19

    # sbfm -> sbfiz
    sbfm w1, w2, #0xD, #0xC
    sbfiz w1, w2, #19, #13
    sbfm x1, x2, #0x3F, #0x3E
    sbfiz x1, x2, #1, #63

    # sbfm -> sbfx
    sbfm w1, w2, #0xC, #0xD
    sbfx w1, w2, #0xC, #0x2
    sbfm x2, x3, #0xC, #0xD
    sbfx x2, x3, #0xC, #0x2

    # sbfm -> sxtb
    sbfm w1, w2, #0, #7
    sxtb w1, w2
    sbfm x2, x3, #0, #7
    sxtb x2, w3

    # sbfm -> sxth
    sbfm w1, w2, #0, #15
    sxth w1, w2
    sbfm x2, x3, #0, #15
    sxth x2, w3

    # sbfm -> sxtw
    sbfm x2, x3, #0, #31
    sxtw x2, w3
.size sbfm, . - sbfm

bfm:
    # bfm -> bfi
    bfm w1, w2, #0xD, #0xC
    bfi w1, w2, #0x13, #0xD
    bfm x1, x2, #0xD, #0xC
    bfi x1, x2, #0x33, #0xD

    # bfm -> bfxil
    bfm w1, w2, #0xC, #0xD
    bfxil w1, w2, #0xC, #0x2
    bfm x1, x2, #0xC, #0xD
    bfxil x1, x2, #0xC, #0x2
.size bfm, . - bfm

ubfm:
    # ubfm -> lsl
    ubfm w1, w2, #0xE, #0xD
    lsl w1, w2, #0x12
    ubfm x2, x3, #0x20, #0x1F
    lsl x2, x3, #0x20

    # ubfm -> lsr
    ubfm w1, w2, #0xE, #0x1F
    lsr w1, w2, #0xE
    ubfm x2, x3, #0x1D, #0x3F
    lsr x2, x3, #0x1D

    # ubfm -> ubfiz
    ubfm w1, w2, #0xE, #0xC
    ubfiz w1, w2, #0x12, #0xD
    ubfm x2, x3, #0x1E, #0x1A
    ubfiz x2, x3, #0x22, #0x1B

    # ubfm -> ubfx
    ubfm w1, w2, #0xE, #0xF
    ubfx w1, w2, #0xE, #0x2
    ubfm x2, x3, #0x1E, #0x2E
    ubfx x2, x3, #0x1E, #0x11

    # ubfm -> uxtb
    ubfm w1, w2, #0x0, #0x7
    uxtb w1, w2
    ubfm w1, w2, #0x0, #0xF
    uxth w1, w2
.size ubfm, . - ubfm

extr:
    extr w1, w2, w3, #13
    extr x2, x3, x4, #31

    extr w1, w2, w2, #13
    ror w1, w2, #13

    extr x1, x2, x2, #31
    ror x1, x2, #31
.size extr, . - extr

label1:
    b .
.size label1, . - label1
