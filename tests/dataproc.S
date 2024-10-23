.section .text
dp_2src:
    udiv w1, w2, w3
    udiv x1, x2, x3

    sdiv w1, w2, w3
    sdiv x1, x2, x3

    lslv w1, w2, w3
    lslv x1, x2, x3

    lsrv w1, w2, w3
    lsrv x1, x2, x3

    asrv w1, w2, w3
    asrv x1, x2, x3

    rorv w1, w2, w3
    rorv x1, x2, x3

    # crc32b, crc32h, crc32w, crc32x
    crc32b w1, w2, w3
    crc32h w1, w2, w3
    crc32w w1, w2, w3
    crc32x w1, w2, x3

    # crc32cb, crc32ch, crc32cw, crc32cx
    crc32cb w1, w2, w3
    crc32ch w1, w2, w3
    crc32cw w1, w2, w3
    crc32cx w1, w2, x3

.size dp_2src, . - dp_2src

dp_1src:
    rbit w1, w2
    rbit x1, x2

    rev16 w1, w2
    rev16 x1, x2

    rev w1, w2
    rev x1, x2

    clz w1, w2
    clz x1, x2

    cls w1, w2
    cls x1, x2
.size dp_1src, . - dp_1src

dp_log_shift:
    # TODO other shifts
    and w1, w2, w3, lsl #3
    and x1, x2, x3, asr #3

    bic w1, w2, w3, lsl #3
    bic x1, x2, x3, lsl #3

    orr w1, w2, w3, lsl #3
    orr x1, x2, x3, lsl #3

    orn w1, w2, w3, lsl #3
    orn x1, x2, x3, lsl #3

    eor w1, w2, w3, lsl #3
    eor x1, x2, x3, lsl #3

    eon w1, w2, w3, lsl #3
    eon x1, x2, x3, lsl #3

    ands w1, w2, w3, lsl #3
    ands x1, x2, x3, lsl #3

    bics w1, w2, w3, lsl #3
    bics x1, x2, x3, lsl #3
.size dp_log_shift, . - dp_log_shift

dp_log_shift_alias:
    # orr -> mov
    orr w1, wzr, w2, lsl #0
    mov w1, w2
    orr x1, xzr, x2, lsl #0
    mov x1, x2

    # orn -> mvn
    orn w1, wzr, w2, lsl #0
    mvn w1, w2
    orn x1, xzr, x2, lsl #0
    mvn x1, x2

    # ands -> tst
    ands wzr, w1, w2, lsl #3
    tst w1, w2, lsl #3
    ands xzr, x1, x2, lsl #3
    tst x1, x2, lsl #3
.size dp_log_shift_alias, . - dp_log_shift_alias

dp_add_sub_shreg:
    add w1, w2, w3, lsl #3
    add x1, x2, x3, lsl #3
    adds w1, w2, w3, lsl #3
    adds x1, x2, x3, lsl #3

    sub w1, w2, w3, lsl #3
    sub x1, x2, x3, lsl #3
    subs w1, w2, w3, lsl #3
    subs x1, x2, x3, lsl #3
.size dp_add_sub_shreg, . - dp_add_sub_shreg

dp_add_sub_shreg_alias:
    # adds -> cmn
    adds wzr, w2, w3, lsl #3
    cmn w2, w3, lsl #3
    adds xzr, x2, x3, lsl #3
    cmn x2, x3, lsl #3

    # sub -> neg
    sub w1, wzr, w3, lsl #3
    neg w1, w3, lsl #3
    sub x1, xzr, x3, lsl #3
    neg x1, x3, lsl #3

    # subs -> cmp
    subs wzr, w1, w2, lsl #3
    cmp w1, w2, lsl #3
    subs xzr, x1, x2, lsl #3
    cmp x1, x2, lsl #3

    # subs -> negs
    subs w1, wzr, w2, lsl #3
    negs w1, w2, lsl #3
    subs x1, xzr, x2, lsl #3
    negs x1, x2, lsl #3
.size dp_add_sub_shreg_alias, . - dp_add_sub_shreg_alias

dp_add_sub_exreg:
    add w1, w2, w3, uxtb
    add w1, w2, w3, uxth #1
    add w1, w2, w3, uxtw
    add w1, w2, w3, uxtw #3

    add x1, x2, w3, uxtb
    add x1, x2, w3, uxtw #3
    add x1, x2, w3, sxth
    add x1, x2, x3, sxtx

    sub w1, w2, w3, uxtb
    sub x1, x2, w3, uxtb
    sub x1, x2, x3, uxtx

    adds w1, w2, w3, uxth #1
    adds x1, x2, w3, uxth #1
    adds x1, x2, x3, sxtx

    subs w1, w2, w3, uxth #1
    subs x1, x2, x3, uxth #1
    subs x1, x2, x3, sxtx
.size dp_add_sub_exreg, . - dp_add_sub_exreg

dp_add_sub_exreg_alias:
    # adds -> cmn
    adds wzr, w1, w2, uxtb
    cmn w1, w2, uxtb
    adds xzr, x1, w2, sxtw #1
    cmn x1, w2, sxtw #1
    adds xzr, x1, x2, sxtx
    cmn x1, x2, sxtx

    # subs -> cmp
    subs wzr, w1, w2, uxtb
    cmp w1, w2, uxtb
    subs xzr, x1, w2, sxtw #1
    cmp x1, w2, sxtw #1
    subs xzr, x1, x2, sxtx
    cmp x1, x2, sxtx
.size dp_add_sub_exreg_alias, . - dp_add_sub_exreg_alias

dp_add_sub_carry:
    adc w1, w2, w3
    adc x1, x2, x3

    adcs w1, w2, w3
    adcs x1, x2, x3

    sbc w1, w2, w3
    sbc x1, x2, x3

    sbcs w1, w2, w3
    sbcs x1, x2, x3
.size dp_add_sub_carry, . - dp_add_sub_carry

dp_add_sub_carry_alias:
    # sbc -> ngc
    sbc w1, wzr, w2
    ngc w1, w2
    sbc x1, xzr, x2
    ngc x1, x2

    # sbcs -> ngcs
    sbcs w1, wzr, w2
    ngcs w1, w2
    sbcs x1, xzr, x2
    ngcs x1, x2
.size dp_add_sub_carry_alias, . - dp_add_sub_carry_alias

dp_cond_cmp_reg:
    ccmn w1, w2, #1, ge
    ccmn x1, x2, #2, le

    ccmp w1, w2, #3, lo
    ccmp x1, x2, #4, hi
.size dp_cond_cmp_reg, . - dp_cond_cmp_reg

dp_cond_cmp_imm:
    ccmn w1, #0xD, #1, ge
    ccmn x1, #0xE, #2, le

    ccmp w1, #0x10, #3, lo
    ccmp x1, #0x11, #4, hi
.size dp_cond_cmp_imm, . - dp_cond_cmp_imm

dp_cond_sel:
    csel w1, w2, w3, ge
    csel x1, x2, x3, le

    csinc w1, w2, w3, lo
    csinc x1, x2, x3, hi

    csinv w1, w2, w3, ge
    csinv x1, x2, x3, le

    csneg w1, w2, w3, lo
    csneg x1, x2, x3, hi

.size dp_cond_sel, . - dp_cond_sel

dp_cond_sel_alias:
    # csinc -> cinc
    csinc w1, w2, w2, ge
    cinc w1, w2, lt
    csinc x1, x2, x2, le
    cinc x1, x2, gt

    # csinc -> cset
    csinc w1, wzr, wzr, ge
    cset w1, lt
    csinc x1, xzr, xzr, le
    cset x1, gt

    # csinv -> cinv
    csinv w1, w2, w2, ge
    cinv w1, w2, lt
    csinv x1, x2, x2, le
    cinv x1, x2, lt

    # csinv -> csetm
    csinv w1, wzr, wzr, ge
    csetm w1, lt
    csinv x1, xzr, xzr, le
    csetm x1, gt

    # csneg -> cneg
    csneg w1, w2, w2, ge
    cneg w1, w2, lt
    csneg x1, x2, x2, le
    cneg x1, x2, gt
.size dp_cond_sel_alias, . - dp_cond_sel_alias

dp_3src:
    madd w1, w2, w3, w4
    madd x1, x2, x3, x4

    msub w1, w2, w3, w4
    msub x1, x2, x3, x4

    smaddl x1, w2, w3, x4
    smsubl x1, w2, w3, x4

    smulh x1, x2, x3

    umaddl x1, w2, w3, x4
    umsubl x1, w2, w3, x4

    umulh x1, x2, x3

.size dp_3src, . - dp_3src

dp_3src_alias:
    # madd -> mul
    madd w1, w2, w3, wzr
    mul w1, w2, w3
    madd x1, x2, x3, xzr
    mul x1, x2, x3

    # msub -> mneg
    msub w1, w2, w3, wzr
    mneg w1, w2, w3
    msub x1, x2, x3, xzr
    mneg x1, x2, x3

    # smaddl -> smull
    smaddl x1, w2, w3, xzr
    smull x1, w2, w3

    # smsubl -> smnegl
    smsubl x1, w2, w3, xzr
    smnegl x1, w2, w3

    # umaddl -> umull
    umaddl x1, w2, w3, xzr
    umull x1, w2, w3

    # umsubl -> umnegl
    umsubl x1, w2, w3, xzr
    umnegl x1, w2, w3
.size dp_3src_alias, . - dp_3src_alias
