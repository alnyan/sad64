.section .text
cond_branch_imm:
    b.ne label1
    b.eq label1
    b.cs label1
    b.lo label1
    b.mi label1
    b.pl label1
    b.vs label1
    b.vc label1
    b.hi label1
    b.ls label1
    b.ge label1
    b.lt label1
    b.gt label1
    b.le label1
    b.al label1
    b.nv label1
.size cond_branch_imm, . - cond_branch_imm

exception:
    svc #0
    svc #123

    hvc #0
    hvc #123

    smc #0
    smc #123

    brk #0
    brk #123

    hlt #0
    hlt #123

    dcps1
    dcps1 #123

    dcps2
    dcps2 #321

    dcps3
    dcps3 #111
.size exception, . - exception

system:
    # msr immediate
    msr spsel, #0x0
    msr spsel, #0x1

    msr daifset, #0x0
    msr daifset, #0x1
    msr daifset, #0xF

    msr daifclr, #0x0
    msr daifclr, #0x1
    msr daifclr, #0xF

    hint #0
    nop
    hint #1
    yield
    hint #2
    wfe
    hint #3
    wfi
    hint #4
    sev
    hint #5
    sevl
    hint #6
    dgh
    hint #7
    xpaclri
    hint #123
    clrex
    clrex #0
    clrex #1
    dsb sy
    dsb st
    dsb ld
    dsb ish
    dsb ishst
    dsb nsh
    dsb nshst
    dsb nshld
    dsb osh
    dsb oshst
    dsb oshld
    dmb oshld
    isb
    isb sy

    sys #1, C2, C3, #4
    sys #1, C2, C3, #4, x5

    at s1e1r, x1
    at s12e0w, x2
    at s12e0r, xzr

    dc zva, x1
    dc civac, xzr

    ic ialluis
    ic iallu
    ic ivau, x2

    msr spsr_el1, xzr
    msr elr_el2, x3
    msr dbgbcr3_el1, x3
    msr dbgbcr8_el1, x3
    msr dbgdtrtx_el0, x2
    msr S3_0_c3_c3_0, x3

    sysl x3, #3, c0, c1, #2

    mrs xzr, spsr_el1
    mrs x3, elr_el2
    mrs x3, dbgbcr3_el1
    mrs x2, dbgdtrrx_el0
    mrs x3, S3_0_c3_c3_0
.size system, . - system

uncond_branch_reg:
    # br, blr, ret, eret, drps
    br x3
    br xzr
    blr x3
    blr xzr

    eret
    drps
.size uncond_branch_reg, . - uncond_branch_reg

uncond_branch_imm:
    b label1
    bl label1
.size uncond_branch_imm, . - uncond_branch_imm

cmp_branch_imm:
    cbz w1, label1
    cbnz w2, uncond_branch_imm

1:
    cbz x3, uncond_branch_reg
    cbnz x4, 1b
.size cmp_branch_imm, . - cmp_branch_imm

test_branch_imm:
    tbz w1, #3, label1
    tbnz w3, #31, cmp_branch_imm

    tbz x1, #63, uncond_branch_imm
    tbnz x3, #12, cmp_branch_imm
.size test_branch_imm, . - test_branch_imm

label1:
    b .
.size label1, . - label1
