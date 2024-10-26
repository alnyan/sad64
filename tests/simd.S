.section .text

simd_crypto_aes:
    aese v1.16b, v3.16b
    aesd v1.16b, v3.16b
    aesmc v1.16b, v3.16b
    aesimc v1.16b, v3.16b
.size simd_crypto_aes, . - simd_crypto_aes

simd_crypto_3reg_sha:
    sha1c q1, s2, v3.4s
    sha1p q1, s2, v3.4s
    sha1m q1, s2, v3.4s
    sha1su0 v1.4s, v2.4s, v3.4s
    sha256h q1, q2, v3.4s
    sha256h2 q1, q2, v3.4s
    sha256su1 v1.4s, v2.4s, v3.4s
.size simd_crypto_3reg_sha, . - simd_crypto_3reg_sha

simd_crypto_2reg_sha:
    sha1h s1, s2
    sha1su1 v1.4s, v2.4s
    sha256su0 v1.4s, v2.4s
.size simd_crypto_2reg_sha, . - simd_crypto_2reg_sha

simd_scalar_copy:
    dup b1, v2.b[0]
    dup b1, v2.b[15]
    dup h1, v2.h[0]
    dup h1, v2.h[7]
    dup s1, v2.s[0]
    dup s1, v2.s[3]
    dup d1, v2.d[0]
    dup d1, v2.d[1]
.size simd_scalar_copy, . - simd_scalar_copy

simd_scalar_2reg_misc:
    suqadd b1, b2
    suqadd s2, s3
    sqabs b1, b2
    sqabs d2, d3
    cmgt d1, d2, #0
    cmeq d1, d2, #0
    cmlt d1, d2, #0
    abs d1, d2

    sqxtn b1, h2
    sqxtn h2, s3
    sqxtn s3, d4

    fcvtns d1, d2
    fcvtms s1, s2
    fcvtas d1, d2
    scvtf s1, s2

    fcmgt s1, s2, #0.0
    fcmeq s1, s2, #0.0
    fcmlt d1, d2, #0.0

    fcvtps d1, d2
    fcvtzs s1, s2

    frecpe s1, s2
    frecpx s1, s2

    usqadd b1, b2
    sqneg b1, b2

    cmge d1, d2, #0
    cmle d1, d2, #0

    neg d1, d2

    sqxtun b1, h2
    sqxtun h1, s2
    sqxtun s1, d2

    uqxtn b1, h2
    uqxtn h1, s2
    uqxtn s1, d2

    fcvtxn s1, d2
    fcvtnu s1, s2
    fcvtmu s1, s2
    fcvtau s1, s2
    ucvtf s1, s2

    fcmge s1, s2, #0.0
    fcmle s1, s2, #0.0

    fcvtpu s1, s2
    fcvtzu s1, s2

    frsqrte s1, s2
.size simd_scalar_2reg_misc, . - simd_scalar_2reg_misc

simd_scalar_pairwise:
    addp d1, v2.2d
    fmaxnmp s1, v2.2s
    fmaxnmp d2, v3.2d
    faddp s1, v2.2s
    faddp d2, v3.2d
    fmaxp s1, v2.2s
    fmaxp d2, v3.2d
    fminnmp s1, v2.2s
    fminnmp d2, v3.2d
    fminp s1, v2.2s
    fminp d2, v3.2d
.size simd_scalar_pairwise, . - simd_scalar_pairwise

simd_scalar_3diff_same:
    sqdmlal s1, h2, h3
    sqdmlal d1, s2, s3
    sqdmlsl s1, h2, h3
    sqdmlsl d1, s2, s3
    sqdmull s1, h2, h3
    sqdmull d1, s2, s3

    sqadd b1, b2, b3
    sqadd h1, h2, h3
    sqadd s1, s2, s3
    sqadd d1, d2, d3
    sqsub b1, b2, b3
    sqsub h1, h2, h3
    sqsub s1, s2, s3
    sqsub d1, d2, d3

    cmgt d1, d2, d3
    cmge d1, d2, d3
    sshl d1, d2, d3

    sqshl b1, b2, b3
    sqshl h1, h2, h3
    sqshl s1, s2, s3
    sqshl d1, d2, d3

    srshl d1, d2, d3

    sqrshl b1, b2, b3
    sqrshl h1, h2, h3
    sqrshl s1, s2, s3
    sqrshl d1, d2, d3

    add d1, d2, d3
    cmtst d1, d2, d3

    sqdmulh h1, h2, h3
    sqdmulh s1, s2, s3

    fmulx s1, s2, s3
    fmulx d1, d2, d3

    fcmeq s1, s2, s3
    fcmeq d1, d2, d3
    frecps s1, s2, s3
    frecps d1, d2, d3

    frsqrts s1, s2, s3
    frsqrts d1, d2, d3

    uqadd b1, b2, b3
    uqadd h1, h2, h3
    uqadd s1, s2, s3
    uqadd d1, d2, d3
    uqsub b1, b2, b3
    uqsub h1, h2, h3
    uqsub s1, s2, s3
    uqsub d1, d2, d3

    cmhi d1, d2, d3
    cmhs d1, d2, d3

    ushl d1, d2, d3
    uqshl b1, b2, b3
    uqshl h1, h2, h3
    uqshl s1, s2, s3
    uqshl d1, d2, d3
    urshl d1, d2, d3
    uqrshl b1, b2, b3
    uqrshl h1, h2, h3
    uqrshl s1, s2, s3
    uqrshl d1, d2, d3

    sub d1, d2, d3
    cmeq d1, d2, d3

    sqrdmulh h1, h2, h3
    sqrdmulh s1, s2, s3

    fcmge s1, s2, s3
    fcmge d1, d2, d3
    facge s1, s2, s3
    facge d1, d2, d3
    fabd s1, s2, s3
    fabd d1, d2, d3
    fcmgt s1, s2, s3
    fcmgt d1, d2, d3
    facgt s1, s2, s3
    facgt d1, d2, d3
.size simd_scalar_3diff_same, . - simd_scalar_3diff_same

simd_scalar_shift_by_imm:
    sshr d1, d2, #1
    ssra d1, d2, #2
    srshr d1, d2, #3
    srsra d1, d2, #4
    shl d1, d2, #2

    sqshl b1, b2, #2
    sqshl h1, h2, #4
    sqshl s1, s2, #6
    sqshl d1, d2, #8

    sqshrn b1, h2, #2
    sqshrn h1, s2, #4
    sqshrn s1, d2, #6
    sqrshrn b1, h2, #2
    sqrshrn h1, s2, #4
    sqrshrn s1, d2, #6

    scvtf h1, h2, #2
    scvtf s1, s2, #4
    scvtf d1, d2, #6
    fcvtzs h1, h2, #8
    fcvtzs s1, s2, #10
    fcvtzs d1, d2, #12

    ushr d1, d2, #1
    usra d1, d2, #2
    urshr d1, d2, #3
    ursra d1, d2, #4
    sri d1, d2, #5
    sli d1, d2, #6

    sqshlu b1, b2, #2
    sqshlu h1, h2, #3
    sqshlu s1, s2, #4
    sqshlu d1, d2, #5
    uqshl b1, b2, #6
    uqshl h1, h2, #7
    uqshl s1, s2, #8
    uqshl d1, d2, #9

    sqrshrun b1, h2, #3
    sqrshrun h2, s3, #4
    sqrshrun s3, d4, #5
    uqshrn b1, h2, #3
    uqshrn h2, s3, #4
    uqshrn s3, d4, #5
    uqrshrn b1, h2, #3
    uqrshrn h2, s3, #4
    uqrshrn s3, d4, #5

    ucvtf s1, s2, #3
    ucvtf d1, d2, #4
    fcvtzu s1, s2, #3
    fcvtzu d1, d2, #4
.size simd_scalar_shift_by_imm, . - simd_scalar_shift_by_imm

simd_scalar_x_index:
    sqdmlal s1, h2, v3.h[0]
    sqdmlal s1, h2, v3.h[7]
    sqdmlal d1, s2, v3.s[0]
    sqdmlal d1, s2, v3.s[3]

    sqdmlsl s1, h2, v3.h[0]
    sqdmlsl s1, h2, v3.h[7]
    sqdmlsl d1, s2, v3.s[0]
    sqdmlsl d1, s2, v3.s[3]

    sqdmull s1, h2, v3.h[0]
    sqdmull s1, h2, v3.h[7]
    sqdmull d1, s2, v3.s[0]
    sqdmull d1, s2, v3.s[3]

    sqdmulh h1, h2, v3.h[0]
    sqdmulh s1, s2, v3.s[3]
    sqrdmulh h1, h2, v3.h[0]
    sqrdmulh s1, s2, v3.s[3]

    fmla h1, h2, v3.h[0]
    fmla h1, h2, v3.h[7]
    fmla s1, s2, v3.s[0]
    fmla s1, s2, v3.s[3]
    fmla d1, d2, v3.d[0]
    fmla d1, d2, v3.d[1]

    fmls h1, h2, v3.h[0]
    fmls h1, h2, v3.h[7]
    fmls s1, s2, v3.s[0]
    fmls s1, s2, v3.s[3]
    fmls d1, d2, v3.d[0]
    fmls d1, d2, v3.d[1]

    fmul h1, h2, v3.h[0]
    fmul h1, h2, v3.h[7]
    fmul s1, s2, v3.s[0]
    fmul s1, s2, v3.s[3]
    fmul d1, d2, v3.d[0]
    fmul d1, d2, v3.d[1]

    fmulx h1, h2, v3.h[0]
    fmulx h1, h2, v3.h[7]
    fmulx s1, s2, v3.s[0]
    fmulx s1, s2, v3.s[3]
    fmulx d1, d2, v3.d[0]
    fmulx d1, d2, v3.d[1]
.size simd_scalar_x_index, . - simd_scalar_x_index

simd_table_lookup:
    tbl v1.8b, { v2.16b }, v3.8b
    tbl v1.16b, { v2.16b }, v3.16b
    tbl v1.8b, { v2.16b, v3.16b }, v4.8b
    tbl v1.16b, { v2.16b, v3.16b }, v4.16b
    tbl v1.8b, { v2.16b, v3.16b, v4.16b }, v5.8b
    tbl v1.16b, { v2.16b, v3.16b, v4.16b }, v5.16b
    tbl v1.8b, { v2.16b, v3.16b, v4.16b, v5.16b }, v6.8b
    tbl v1.16b, { v2.16b, v3.16b, v4.16b, v5.16b }, v6.16b
    tbx v1.8b, { v2.16b }, v3.8b
    tbx v1.16b, { v2.16b }, v3.16b
    tbx v1.8b, { v2.16b, v3.16b }, v4.8b
    tbx v1.16b, { v2.16b, v3.16b }, v4.16b
    tbx v1.8b, { v2.16b, v3.16b, v4.16b }, v5.8b
    tbx v1.16b, { v2.16b, v3.16b, v4.16b }, v5.16b
    tbx v1.8b, { v2.16b, v3.16b, v4.16b, v5.16b }, v6.8b
    tbx v1.16b, { v2.16b, v3.16b, v4.16b, v5.16b }, v6.16b
.size simd_table_lookup, . - simd_table_lookup

simd_permute:
    uzp1 v1.8b, v2.8b, v3.8b
    uzp1 v1.16b, v2.16b, v3.16b
    uzp1 v1.4h, v2.4h, v3.4h
    uzp1 v1.8h, v2.8h, v3.8h
    uzp1 v1.2s, v2.2s, v3.2s
    uzp1 v1.4s, v2.4s, v3.4s
    uzp1 v1.2d, v2.2d, v3.2d

    trn1 v1.8b, v2.8b, v3.8b
    trn1 v1.16b, v2.16b, v3.16b
    trn1 v1.4h, v2.4h, v3.4h
    trn1 v1.8h, v2.8h, v3.8h
    trn1 v1.2s, v2.2s, v3.2s
    trn1 v1.4s, v2.4s, v3.4s
    trn1 v1.2d, v2.2d, v3.2d

    zip1 v1.8b, v2.8b, v3.8b
    zip1 v1.16b, v2.16b, v3.16b
    zip1 v1.4h, v2.4h, v3.4h
    zip1 v1.8h, v2.8h, v3.8h
    zip1 v1.2s, v2.2s, v3.2s
    zip1 v1.4s, v2.4s, v3.4s
    zip1 v1.2d, v2.2d, v3.2d

    uzp2 v1.8b, v2.8b, v3.8b
    uzp2 v1.16b, v2.16b, v3.16b
    uzp2 v1.4h, v2.4h, v3.4h
    uzp2 v1.8h, v2.8h, v3.8h
    uzp2 v1.2s, v2.2s, v3.2s
    uzp2 v1.4s, v2.4s, v3.4s
    uzp2 v1.2d, v2.2d, v3.2d

    trn2 v1.8b, v2.8b, v3.8b
    trn2 v1.16b, v2.16b, v3.16b
    trn2 v1.4h, v2.4h, v3.4h
    trn2 v1.8h, v2.8h, v3.8h
    trn2 v1.2s, v2.2s, v3.2s
    trn2 v1.4s, v2.4s, v3.4s
    trn2 v1.2d, v2.2d, v3.2d

    zip2 v1.8b, v2.8b, v3.8b
    zip2 v1.16b, v2.16b, v3.16b
    zip2 v1.4h, v2.4h, v3.4h
    zip2 v1.8h, v2.8h, v3.8h
    zip2 v1.2s, v2.2s, v3.2s
    zip2 v1.4s, v2.4s, v3.4s
    zip2 v1.2d, v2.2d, v3.2d
.size simd_permute, . - simd_permute

simd_extract:
    ext v1.8b, v2.8b, v3.8b, #0
    ext v1.8b, v2.8b, v3.8b, #7
    ext v1.16b, v2.16b, v3.16b, #0
    ext v1.16b, v2.16b, v3.16b, #15
.size simd_extract, . - simd_extract

simd_copy:
    dup v1.8b, v2.b[0]
    dup v1.16b, v2.b[15]
    dup v1.4h, v2.h[0]
    dup v1.8h, v2.h[7]
    dup v1.2s, v2.s[0]
    dup v1.4s, v2.s[3]
    dup v1.2d, v2.d[0]

    dup v1.8b, w2
    dup v1.16b, w2
    dup v1.4h, w2
    dup v1.8h, w2
    dup v1.2s, w2
    dup v1.4s, w2
    dup v1.2d, x2

    smov w0, v1.b[0]
    smov w0, v1.b[15]
    smov w0, v1.h[0]
    smov w0, v1.h[7]
    smov x0, v1.s[0]
    smov x0, v1.s[3]

    umov w0, v1.b[0]
    umov w0, v1.b[15]
    umov w0, v1.h[0]
    umov w0, v1.h[7]
.size simd_copy, . - simd_copy

simd_copy_alias:
    # umov wM, vN.s, umov xM, vN.d -> mov
    umov w0, v1.s[0]
    mov w0, v1.s[0]
    umov w0, v1.s[3]
    mov w0, v1.s[3]

    umov x0, v1.d[0]
    mov x0, v1.d[0]
    umov x0, v1.d[1]
    mov x0, v1.d[1]

    # ins -> mov (always)
    ins v1.b[15], w0
    mov v1.b[15], w0
    ins v1.h[7], w0
    mov v1.h[7], w0
    ins v1.s[3], w0
    mov v1.s[3], w0
    ins v1.d[1], x0
    mov v1.d[1], x0

    ins v1.b[7], v2.b[15]
    mov v1.b[7], v2.b[15]
    ins v1.h[3], v2.h[7]
    mov v1.h[3], v2.h[7]
    ins v1.s[1], v2.s[3]
    mov v1.s[1], v2.s[3]
    ins v1.d[0], v2.d[1]
    mov v1.d[0], v2.d[1]
.size simd_copy_alias, . - simd_copy_alias

simd_2reg_misc:
    rev64 v1.8b, v2.8b
    rev64 v1.16b, v2.16b
    rev64 v1.2s, v2.2s
    rev64 v1.4s, v2.4s

    rev16 v1.8b, v2.8b
    rev16 v1.16b, v2.16b

    saddlp v1.8h, v2.16b
    saddlp v1.4h, v2.8b
    saddlp v1.4s, v2.8h
    saddlp v1.2d, v2.4s

    suqadd v1.8b, v2.8b
    suqadd v1.16b, v2.16b
    suqadd v1.4h, v2.4h
    suqadd v1.4s, v2.4s
    suqadd v1.2d, v2.2d

    cls v1.8b, v2.8b
    cls v1.16b, v2.16b
    cls v1.4h, v2.4h
    cls v1.2s, v2.2s

    cnt v1.8b, v2.8b
    cnt v1.16b, v2.16b

    sadalp v1.4h, v2.8b
    sadalp v1.8h, v2.16b
    sadalp v1.2s, v2.4h
    sadalp v1.4s, v2.8h
    sadalp v1.2d, v2.4s

    sqabs v1.8b, v2.8b
    sqabs v1.16b, v2.16b
    sqabs v1.4h, v2.4h
    sqabs v1.4s, v2.4s
    sqabs v1.2d, v2.2d

    cmgt v1.8b, v2.8b, #0
    cmgt v1.16b, v2.16b, #0
    cmgt v1.2d, v2.2d, #0
    cmeq v1.8b, v2.8b, #0
    cmeq v1.16b, v2.16b, #0
    cmeq v1.2d, v2.2d, #0
    cmlt v1.8b, v2.8b, #0
    cmlt v1.16b, v2.16b, #0
    cmlt v1.2d, v2.2d, #0

    abs v1.8b, v2.8b
    abs v1.16b, v2.16b
    abs v1.2d, v2.2d

    xtn v1.8b, v2.8h
    xtn v1.4h, v2.4s
    xtn v1.2s, v2.2d
    xtn2 v1.16b, v2.8h
    xtn2 v1.8h, v2.4s
    xtn2 v1.4s, v2.2d

    sqxtn v1.8b, v2.8h
    sqxtn v1.2s, v2.2d
    sqxtn2 v1.16b, v2.8h
    sqxtn2 v1.4s, v2.2d

    fcvtn v1.4h, v2.4s
    fcvtn v1.2s, v2.2d
    fcvtn2 v1.8h, v2.4s
    fcvtn2 v1.4s, v2.2d

    fcvtl v1.4s, v2.4h
    fcvtl v1.2d, v2.2s
    fcvtl2 v1.4s, v2.8h
    fcvtl2 v1.2d, v2.4s

    frintn v1.2s, v2.2s
    frintn v1.4s, v2.4s
    frintn v1.2d, v2.2d
    frintm v1.2s, v2.2s
    frintm v1.4s, v2.4s
    frintm v1.2d, v2.2d
    fcvtns v1.2s, v2.2s
    fcvtns v1.4s, v2.4s
    fcvtns v1.2d, v2.2d
    fcvtms v1.2s, v2.2s
    fcvtms v1.4s, v2.4s
    fcvtms v1.2d, v2.2d
    fcvtas v1.2s, v2.2s
    fcvtas v1.4s, v2.4s
    fcvtas v1.2d, v2.2d
    scvtf v1.2s, v2.2s
    scvtf v1.4s, v2.4s
    scvtf v1.2d, v2.2d

    fcmgt v1.2s, v2.2s, #0
    fcmgt v1.2d, v2.2d, #0
    fcmeq v1.2s, v2.2s, #0
    fcmeq v1.2d, v2.2d, #0
    fcmlt v1.2s, v2.2s, #0
    fcmlt v1.2d, v2.2d, #0

    fabs v1.2s, v2.2s
    fabs v1.2d, v2.2d

    frintp v1.2s, v2.2s
    frintp v1.4s, v2.4s
    frintp v1.2d, v2.2d
    frintz v1.2s, v2.2s
    frintz v1.4s, v2.4s
    frintz v1.2d, v2.2d
    fcvtps v1.2s, v2.2s
    fcvtps v1.2d, v2.2d
    fcvtzs v1.4s, v2.4s
    fcvtzs v1.2d, v2.2d

    urecpe v1.2s, v2.2s
    urecpe v1.4s, v2.4s
    frecpe v1.2s, v2.2s
    frecpe v1.4s, v2.4s
    frecpe v1.2d, v2.2d

    rev32 v1.8b, v2.8b
    rev32 v1.16b, v2.16b
    rev32 v1.4h, v2.4h
    rev32 v1.8h, v2.8h

    uaddlp v1.4h, v2.8b
    uaddlp v1.8h, v2.16b
    uaddlp v1.1d, v2.2s
    uaddlp v1.2d, v2.4s

    usqadd v1.8b, v2.8b
    usqadd v1.16b, v2.16b
    usqadd v1.4h, v2.4h
    usqadd v1.4s, v2.4s
    usqadd v1.2d, v2.2d

    clz v1.8b, v2.8b
    clz v1.16b, v2.16b
    clz v1.4h, v2.4h
    clz v1.4s, v2.4s

    uadalp v1.4h, v2.8b
    uadalp v1.8h, v2.16b
    uadalp v1.2s, v2.4h
    uadalp v1.4s, v2.8h
    uadalp v1.1d, v2.2s
    uadalp v1.2d, v2.4s

    sqneg v1.8b, v2.8b
    sqneg v1.16b, v2.16b
    sqneg v1.4h, v2.4h
    sqneg v1.4s, v2.4s
    sqneg v1.2d, v2.2d

    cmge v1.8b, v2.8b, #0
    cmge v1.16b, v2.16b, #0
    cmge v1.4h, v2.4h, #0
    cmge v1.4s, v2.4s, #0
    cmge v1.2d, v2.2d, #0
    cmle v1.8b, v2.8b, #0
    cmle v1.16b, v2.16b, #0
    cmle v1.4h, v2.4h, #0
    cmle v1.4s, v2.4s, #0
    cmle v1.2d, v2.2d, #0

    neg v1.8b, v2.8b
    neg v1.16b, v2.16b
    neg v1.4h, v2.4h
    neg v1.4s, v2.4s
    neg v1.2d, v2.2d

    sqxtun v1.8b, v2.8h
    sqxtun v1.4h, v2.4s
    sqxtun v1.2s, v2.2d
    sqxtun2 v1.16b, v2.8h

    fcvtxn v1.2s, v2.2d
    fcvtxn2 v1.4s, v2.2d

    frinta v1.2s, v2.2s
    frinta v1.4s, v2.4s
    frinta v1.2d, v2.2d
    frintx v1.2s, v2.2s
    frintx v1.4s, v2.4s
    frintx v1.2d, v2.2d
    fcvtnu v1.2s, v2.2s
    fcvtnu v1.4s, v2.4s
    fcvtnu v1.2d, v2.2d
    fcvtmu v1.2s, v2.2s
    fcvtmu v1.4s, v2.4s
    fcvtmu v1.2d, v2.2d
    fcvtau v1.2s, v2.2s
    fcvtau v1.4s, v2.4s
    fcvtau v1.2d, v2.2d
    ucvtf v1.2s, v2.2s
    ucvtf v1.4s, v2.4s
    ucvtf v1.2d, v2.2d

    not v1.8b, v2.8b
    not v1.16b, v2.16b
    rbit v1.8b, v2.8b
    rbit v1.16b, v2.16b

    fcmge v1.2s, v2.2s, #0.0
    fcmge v1.2d, v2.2d, #0.0
    fcmle v1.2s, v2.2s, #0.0
    fcmle v1.2d, v2.2d, #0.0
    fneg v1.2s, v2.2s
    fneg v1.2d, v2.2d
    frinti v1.2s, v2.2s
    frinti v1.2d, v2.2d
    fcvtpu v1.2s, v2.2s
    fcvtpu v1.2d, v2.2d
    fcvtzu v1.2s, v2.2s
    fcvtzu v1.2d, v2.2d
    ursqrte v1.2s, v2.2s
    ursqrte v1.4s, v2.4s
    frsqrte v1.2s, v2.2s
    frsqrte v1.4s, v2.4s
    frsqrte v1.2d, v2.2d
    fsqrt v1.2s, v2.2s
    fsqrt v1.4s, v2.4s
    fsqrt v1.2d, v2.2d
.size simd_2reg_misc, . - simd_2reg_misc

simd_across_lanes:
    saddlv h1, v1.8b
    saddlv h1, v1.16b
    saddlv d1, v1.4s

    smaxv b1, v1.8b
    smaxv b1, v1.16b
    smaxv h1, v1.8h
    smaxv s1, v1.4s
    sminv b1, v1.8b
    sminv b1, v1.16b
    sminv h1, v1.8h
    sminv s1, v1.4s

    addv b1, v1.8b
    addv b1, v1.16b
    addv h1, v1.8h
    addv s1, v1.4s

    uaddlv h1, v1.8b
    uaddlv h1, v1.16b
    uaddlv d1, v1.4s

    umaxv b1, v1.8b
    umaxv b1, v1.16b
    umaxv h1, v1.8h
    umaxv s1, v1.4s
    uminv b1, v1.8b
    uminv b1, v1.16b
    uminv h1, v1.8h
    uminv s1, v1.4s

    fmaxnmv h1, v1.8h
    fmaxnmv s1, v1.4s
    fmaxv h1, v1.8h
    fmaxv s1, v1.4s
    fminnmv h1, v1.8h
    fminnmv s1, v1.4s
    fminv h1, v1.8h
    fminv s1, v1.4s
.size simd_across_lanes, . - simd_across_lanes

simd_3diff:
    saddl v1.8h, v2.8b, v3.8b
    saddl v1.4s, v2.4h, v3.4h
    saddl v1.2d, v2.2s, v3.2s
    saddl2 v1.8h, v2.16b, v3.16b

    saddw v1.8h, v2.8h, v3.8b
    saddw v1.4s, v2.4s, v3.4h
    saddw v1.2d, v2.2d, v3.2s
    saddw2 v1.8h, v2.8h, v3.16b

    ssubl v1.8h, v2.8b, v3.8b
    ssubl v1.4s, v2.4h, v3.4h
    ssubl v1.2d, v2.2s, v3.2s
    ssubl2 v1.8h, v2.16b, v3.16b

    ssubw v1.8h, v2.8h, v3.8b
    ssubw v1.4s, v2.4s, v3.4h
    ssubw v1.2d, v2.2d, v3.2s
    ssubw2 v1.8h, v2.8h, v3.16b

    addhn v1.8b, v2.8h, v3.8h
    addhn v1.4h, v2.4s, v3.4s
    addhn v1.2s, v2.2d, v3.2d
    addhn2 v1.16b, v2.8h, v3.8h

    sabal v1.8h, v2.8b, v3.8b
    sabal v1.4s, v2.4h, v3.4h
    sabal v1.2d, v2.2s, v3.2s
    sabal2 v1.8h, v2.16b, v3.16b

    subhn v1.8b, v2.8h, v3.8h
    subhn v1.4h, v2.4s, v3.4s
    subhn v1.2s, v2.2d, v3.2d
    subhn2 v1.16b, v2.8h, v3.8h

    sabdl v1.8h, v2.8b, v3.8b
    sabdl v1.4s, v2.4h, v3.4h
    sabdl v1.2d, v2.2s, v3.2s
    sabdl2 v1.8h, v2.16b, v3.16b

    smlal v1.8h, v2.8b, v3.8b
    smlal v1.4s, v2.4h, v3.4h
    smlal v1.2d, v2.2s, v3.2s
    smlal2 v1.8h, v2.16b, v3.16b

    sqdmlal v1.4s, v2.4h, v3.4h
    sqdmlal v1.2d, v2.2s, v3.2s
    sqdmlal2 v1.4s, v2.8h, v3.8h

    smlsl v1.8h, v2.8b, v3.8b
    smlsl v1.4s, v2.4h, v3.4h
    smlsl v1.2d, v2.2s, v3.2s
    smlsl2 v1.8h, v2.16b, v3.16b

    sqdmlsl v1.4s, v2.4h, v3.4h
    sqdmlsl v1.2d, v2.2s, v3.2s
    sqdmlsl2 v1.4s, v2.8h, v3.8h

    smull v1.8h, v2.8b, v3.8b
    smull v1.4s, v2.4h, v3.4h
    smull v1.2d, v2.2s, v3.2s
    smull2 v1.8h, v2.16b, v3.16b

    sqdmull v1.4s, v2.4h, v3.4h
    sqdmull v1.2d, v2.2s, v3.2s
    sqdmull2 v1.4s, v2.8h, v3.8h

    pmull v1.8h, v2.8b, v2.8b
    pmull2 v1.8h, v2.16b, v3.16b

    uaddl v1.8h, v2.8b, v3.8b
    uaddl v1.4s, v2.4h, v3.4h
    uaddl v1.2d, v2.2s, v3.2s
    uaddl2 v1.8h, v2.16b, v3.16b

    uaddw v1.8h, v2.8h, v3.8b
    uaddw v1.4s, v2.4s, v3.4h
    uaddw v1.2d, v2.2d, v3.2s
    uaddw2 v1.8h, v2.8h, v3.16b

    usubl v1.8h, v2.8b, v3.8b
    usubl v1.4s, v2.4h, v3.4h
    usubl v1.2d, v2.2s, v3.2s
    usubl2 v1.8h, v2.16b, v3.16b

    usubw v1.8h, v2.8h, v3.8b
    usubw v1.4s, v2.4s, v3.4h
    usubw v1.2d, v2.2d, v3.2s
    usubw2 v1.8h, v2.8h, v3.16b

    raddhn v1.8b, v2.8h, v3.8h
    raddhn v1.4h, v2.4s, v3.4s
    raddhn v1.2s, v2.2d, v3.2d
    raddhn2 v1.16b, v2.8h, v3.8h

    uabal v1.8h, v2.8b, v3.8b
    uabal v1.4s, v2.4h, v3.4h
    uabal v1.2d, v2.2s, v3.2s
    uabal2 v1.8h, v2.16b, v3.16b

    rsubhn v1.8b, v2.8h, v3.8h
    rsubhn v1.4h, v2.4s, v3.4s
    rsubhn v1.2s, v2.2d, v3.2d
    rsubhn2 v1.16b, v2.8h, v3.8h

    uabdl v1.8h, v2.8b, v3.8b
    uabdl v1.4s, v2.4h, v3.4h
    uabdl v1.2d, v2.2s, v3.2s
    uabdl2 v1.8h, v2.16b, v3.16b

    umull v1.8h, v2.8b, v3.8b
    umull v1.4s, v2.4h, v3.4h
    umull v1.2d, v2.2s, v3.2s
    umull2 v1.8h, v2.16b, v3.16b
.size simd_3diff, . - simd_3diff

simd_3same:
    shadd v1.8b, v2.8b, v3.8b
    shadd v1.4s, v2.4s, v3.4s
    sqadd v1.8b, v2.8b, v3.8b
    sqadd v1.4s, v2.4s, v3.4s
    srhadd v1.8b, v2.8b, v3.8b
    srhadd v1.4s, v2.4s, v3.4s
    shsub v1.8b, v2.8b, v3.8b
    shsub v1.4s, v2.4s, v3.4s
    sqsub v1.8b, v2.8b, v3.8b
    sqsub v1.4s, v2.4s, v3.4s
    cmgt v1.8b, v2.8b, v3.8b
    cmgt v1.2d, v2.2d, v3.2d
    cmge v1.8b, v2.8b, v3.8b
    cmge v1.2d, v2.2d, v3.2d
    sshl v1.8b, v2.8b, v3.8b
    sshl v1.2d, v2.2d, v3.2d
    sqshl v1.8b, v2.8b, v3.8b
    sqshl v1.2d, v2.2d, v3.2d
    srshl v1.8b, v2.8b, v3.8b
    srshl v1.2d, v2.2d, v3.2d
    sqrshl v1.8b, v2.8b, v3.8b
    sqrshl v1.2d, v2.2d, v3.2d
    smax v1.8b, v2.8b, v3.8b
    smax v1.4s, v2.4s, v3.4s
    smin v1.8b, v2.8b, v3.8b
    smin v1.4s, v2.4s, v3.4s
    sabd v1.16b, v2.16b, v3.16b
    sabd v1.4s, v2.4s, v3.4s
    saba v1.16b, v2.16b, v3.16b
    saba v1.4s, v2.4s, v3.4s
    add v1.16b, v2.16b, v3.16b
    add v1.4s, v2.4s, v3.4s
    cmtst v1.16b, v2.16b, v3.16b
    cmtst v1.4s, v2.4s, v3.4s
    mla v1.16b, v2.16b, v3.16b
    mla v1.4s, v2.4s, v3.4s
    mul v1.16b, v2.16b, v3.16b
    mul v1.4s, v2.4s, v3.4s
    smaxp v1.16b, v2.16b, v3.16b
    smaxp v1.4s, v2.4s, v3.4s
    sminp v1.16b, v2.16b, v3.16b
    sminp v1.4s, v2.4s, v3.4s
    sqdmulh v1.8h, v2.8h, v3.8h
    sqdmulh v1.4s, v2.4s, v3.4s
    addp v1.8h, v2.8h, v3.8h
    addp v1.4s, v2.4s, v3.4s
    fmaxnm v1.4s, v2.4s, v3.4s
    fmaxnm v1.2d, v2.2d, v3.2d
    fmla v1.4s, v2.4s, v3.4s
    fmla v1.2d, v2.2d, v3.2d
    fadd v1.4s, v2.4s, v3.4s
    fadd v1.2d, v2.2d, v3.2d
    fmulx v1.4s, v2.4s, v3.4s
    fmulx v1.2d, v2.2d, v3.2d
    fcmeq v1.4s, v2.4s, v3.4s
    fcmeq v1.2d, v2.2d, v3.2d
    fmax v1.4s, v2.4s, v3.4s
    fmax v1.2d, v2.2d, v3.2d
    frecps v1.4s, v2.4s, v3.4s
    frecps v1.2d, v2.2d, v3.2d
    and v1.8b, v2.8b, v3.8b
    and v1.16b, v2.16b, v3.16b
    bic v1.8b, v2.8b, v3.8b
    bic v1.16b, v2.16b, v3.16b
    fminnm v1.4s, v2.4s, v3.4s
    fminnm v1.2d, v2.2d, v3.2d
    fmls v1.4s, v2.4s, v3.4s
    fmls v1.2d, v2.2d, v3.2d
    fsub v1.4s, v2.4s, v3.4s
    fsub v1.2d, v2.2d, v3.2d
    fmin v1.4s, v2.4s, v3.4s
    fmin v1.2d, v2.2d, v3.2d
    frsqrts v1.4s, v2.4s, v3.4s
    frsqrts v1.2d, v2.2d, v3.2d
    orr v1.8b, v2.8b, v3.8b
    orr v1.16b, v2.16b, v3.16b
    orn v1.8b, v2.8b, v3.8b
    orn v1.16b, v2.16b, v3.16b

    uhadd v1.8b, v2.8b, v3.8b
    uhadd v1.4s, v2.4s, v3.4s
    uqadd v1.8b, v2.8b, v3.8b
    uqadd v1.4s, v2.4s, v3.4s
    urhadd v1.8b, v2.8b, v3.8b
    urhadd v1.4s, v2.4s, v3.4s
    uhsub v1.8b, v2.8b, v3.8b
    uhsub v1.4s, v2.4s, v3.4s
    uqsub v1.8b, v2.8b, v3.8b
    uqsub v1.4s, v2.4s, v3.4s
    cmhi v1.8b, v2.8b, v3.8b
    cmhi v1.2d, v2.2d, v3.2d
    cmhs v1.8b, v2.8b, v3.8b
    cmhs v1.2d, v2.2d, v3.2d
    ushl v1.8b, v2.8b, v3.8b
    ushl v1.2d, v2.2d, v3.2d
    uqshl v1.8b, v2.8b, v3.8b
    uqshl v1.2d, v2.2d, v3.2d
    urshl v1.8b, v2.8b, v3.8b
    urshl v1.2d, v2.2d, v3.2d
    uqrshl v1.8b, v2.8b, v3.8b
    uqrshl v1.2d, v2.2d, v3.2d
    umax v1.8b, v2.8b, v3.8b
    umax v1.4s, v2.4s, v3.4s
    umin v1.8b, v2.8b, v3.8b
    umin v1.4s, v2.4s, v3.4s
    uabd v1.16b, v2.16b, v3.16b
    uabd v1.4s, v2.4s, v3.4s
    uaba v1.16b, v2.16b, v3.16b
    uaba v1.4s, v2.4s, v3.4s
    sub v1.16b, v2.16b, v3.16b
    sub v1.4s, v2.4s, v3.4s
    cmeq v1.16b, v2.16b, v3.16b
    cmeq v1.4s, v2.4s, v3.4s
    mls v1.16b, v2.16b, v3.16b
    mls v1.4s, v2.4s, v3.4s
    pmul v1.16b, v2.16b, v3.16b
    pmul v1.8b, v2.8b, v3.8b
    umaxp v1.16b, v2.16b, v3.16b
    umaxp v1.4s, v2.4s, v3.4s
    uminp v1.16b, v2.16b, v3.16b
    uminp v1.4s, v2.4s, v3.4s
    sqrdmulh v1.8h, v2.8h, v3.8h
    sqrdmulh v1.4s, v2.4s, v3.4s
    fmaxnmp v1.4s, v2.4s, v3.4s
    fmaxnmp v1.2d, v2.2d, v3.2d
    faddp v1.4s, v2.4s, v3.4s
    faddp v1.2d, v2.2d, v3.2d
    fmul v1.4s, v2.4s, v3.4s
    fmul v1.2d, v2.2d, v3.2d
    fcmge v1.4s, v2.4s, v3.4s
    fcmge v1.2d, v2.2d, v3.2d
    facge v1.4s, v2.4s, v3.4s
    facge v1.2d, v2.2d, v3.2d
    fmaxp v1.4s, v2.4s, v3.4s
    fmaxp v1.2d, v2.2d, v3.2d
    fdiv v1.4s, v2.4s, v3.4s
    fdiv v1.2d, v2.2d, v3.2d
    eor v1.8b, v2.8b, v3.8b
    eor v1.16b, v2.16b, v3.16b
    bsl v1.8b, v2.8b, v3.8b
    bsl v1.16b, v2.16b, v3.16b
    fminnmp v1.4s, v2.4s, v3.4s
    fminnmp v1.2d, v2.2d, v3.2d
    fabd v1.4s, v2.4s, v3.4s
    fabd v1.2d, v2.2d, v3.2d
    fcmgt v1.4s, v2.4s, v3.4s
    fcmgt v1.2d, v2.2d, v3.2d
    facgt v1.4s, v2.4s, v3.4s
    facgt v1.2d, v2.2d, v3.2d
    fminp v1.4s, v2.4s, v3.4s
    fminp v1.2d, v2.2d, v3.2d
    bit v1.8b, v2.8b, v3.8b
    bit v1.16b, v2.16b, v3.16b
    bif v1.8b, v2.8b, v3.8b
    bif v1.16b, v2.16b, v3.16b
.size simd_3same, . - simd_3same

simd_imm:
    movi v1.4s, #0x12, lsl #24
    orr v1.4s, #0x12, lsl #24
    movi v1.8h, #0x7F, lsl #8
    orr v1.8h, #0x7F, lsl #8
    movi v1.4s, #0x12, msl #16
    movi v1.16b, #0x1F
    fmov v1.2s, #4.0

    mvni v1.4s, #0x12, lsl #24
    bic v1.4s, #0x12, lsl #24
    mvni v1.8h, #0x7F, lsl #8
    bic v1.8h, #0x7F, lsl #8
    mvni v1.4s, #0x12, msl #16
    fmov v1.2d, #16.0

    movi v1.2d, #0
    movi d1, #0
.size simd_imm, . - simd_imm

simd_shift_by_imm:
    sshr v1.16b, v2.16b, #3
    sshr v1.2d, v2.2d, #3
    ssra v1.16b, v2.16b, #3
    ssra v1.2d, v2.2d, #3
    srshr v1.16b, v2.16b, #3
    srshr v1.2d, v2.2d, #3
    srsra v1.16b, v2.16b, #3
    srsra v1.2d, v2.2d, #3
    shl v1.16b, v2.16b, #3
    shl v1.2d, v2.2d, #3
    sqshl v1.16b, v2.16b, #3
    sqshl v1.2d, v2.2d, #3
    shrn v1.8b, v2.8h, #3
    shrn v1.2s, v2.2d, #3
    shrn2 v1.16b, v2.8h, #3
    shrn2 v1.4s, v2.2d, #3
    rshrn v1.8b, v2.8h, #3
    rshrn v1.2s, v2.2d, #3
    rshrn2 v1.16b, v2.8h, #3
    rshrn2 v1.4s, v2.2d, #3
    sqshrn v1.8b, v2.8h, #3
    sqshrn v1.2s, v2.2d, #3
    sqshrn2 v1.16b, v2.8h, #3
    sqshrn2 v1.4s, v2.2d, #3
    sqrshrn v1.8b, v2.8h, #3
    sqrshrn v1.2s, v2.2d, #3
    sqrshrn2 v1.16b, v2.8h, #3
    sqrshrn2 v1.4s, v2.2d, #3
    sshll v1.8h, v2.8b, #3
    sshll v1.2d, v2.2s, #3
    sshll2 v1.8h, v2.16b, #3
    sshll2 v1.2d, v2.4s, #3
    scvtf v1.4h, v2.4h, #3
    scvtf v1.2s, v2.2s, #3
    scvtf v1.4s, v2.4s, #3
    scvtf v1.2d, v2.2d, #3
    fcvtzs v1.4h, v2.4h, #3
    fcvtzs v1.2d, v2.2d, #3

    ushr v1.16b, v2.16b, #3
    ushr v1.2d, v2.2d, #3
    usra v1.16b, v2.16b, #3
    usra v1.2d, v2.2d, #3
    urshr v1.16b, v2.16b, #3
    urshr v1.2d, v2.2d, #3
    ursra v1.16b, v2.16b, #3
    ursra v1.2d, v2.2d, #3
    sri v1.16b, v2.16b, #3
    sri v1.2d, v2.2d, #3
    sli v1.16b, v2.16b, #3
    sli v1.2d, v2.2d, #3
    sqshlu v1.16b, v2.16b, #3
    sqshlu v1.2d, v2.2d, #3
    uqshl v1.16b, v2.16b, #3
    uqshl v1.2d, v2.2d, #3
    sqshrun v1.8b, v2.8h, #3
    sqshrun v1.2s, v2.2d, #3
    sqshrun2 v1.16b, v2.8h, #3
    sqshrun2 v1.4s, v2.2d, #3
    sqrshrun v1.8b, v2.8h, #3
    sqrshrun v1.2s, v2.2d, #3
    sqrshrun2 v1.16b, v2.8h, #3
    sqrshrun2 v1.4s, v2.2d, #3
    uqshrn v1.8b, v2.8h, #3
    uqshrn v1.2s, v2.2d, #3
    uqshrn2 v1.16b, v2.8h, #3
    uqshrn2 v1.4s, v2.2d, #3
    uqrshrn v1.8b, v2.8h, #3
    uqrshrn v1.2s, v2.2d, #3
    uqrshrn2 v1.16b, v2.8h, #3
    uqrshrn2 v1.4s, v2.2d, #3
    ushll v1.8h, v2.8b, #3
    ushll v1.2d, v2.2s, #3
    ushll2 v1.8h, v2.16b, #3
    ushll2 v1.2d, v2.4s, #3
    ucvtf v1.4h, v2.4h, #3
    ucvtf v1.2d, v2.2d, #3
    fcvtzu v1.4h, v2.4h, #3
    fcvtzu v1.2d, v2.2d, #3
.size simd_shift_by_imm, . - simd_shift_by_imm

simd_vector_x_index:
    smlal v1.4s, v2.4h, v3.h[1]
    smlal v1.2d, v2.2s, v3.s[2]
    smlal2 v1.4s, v2.8h, v3.h[3]
    sqdmlal v1.4s, v2.4h, v3.h[1]
    sqdmlal v1.2d, v2.2s, v3.s[2]
    sqdmlal2 v1.4s, v2.8h, v3.h[3]
    smlsl v1.4s, v2.4h, v3.h[1]
    smlsl v1.2d, v2.2s, v3.s[2]
    smlsl2 v1.4s, v2.8h, v3.h[3]
    sqdmlsl v1.4s, v2.4h, v3.h[1]
    sqdmlsl v1.2d, v2.2s, v3.s[2]
    sqdmlsl2 v1.4s, v2.8h, v3.h[3]
    mul v1.4h, v2.4h, v3.h[1]
    mul v1.4s, v2.4s, v3.s[2]
    smull v1.4s, v2.4h, v3.h[1]
    smull v1.2d, v2.2s, v3.s[2]
    smull2 v1.4s, v2.8h, v3.h[3]
    sqdmull v1.4s, v2.4h, v3.h[1]
    sqdmull v1.2d, v2.2s, v3.s[2]
    sqdmull2 v1.4s, v2.8h, v3.h[3]
    sqdmulh v1.4h, v2.4h, v3.h[1]
    sqdmulh v1.2s, v2.2s, v3.s[2]
    sqrdmulh v1.4h, v2.4h, v3.h[1]
    sqrdmulh v1.2s, v2.2s, v3.s[2]
    fmla v1.2s, v2.2s, v3.s[2]
    fmla v1.2d, v2.2d, v3.d[0]
    fmls v1.2s, v2.2s, v3.s[2]
    fmls v1.2d, v2.2d, v3.d[0]
    fmul v1.2s, v2.2s, v3.s[2]
    fmul v1.2d, v2.2d, v3.d[0]
    mla v1.2s, v2.2s, v3.s[2]
    umlal v1.4s, v2.4h, v3.h[1]
    umlal v1.2d, v2.2s, v3.s[2]
    umlal2 v1.4s, v2.8h, v3.h[3]
    mls v1.2s, v2.2s, v3.s[2]
    umlsl v1.4s, v2.4h, v3.h[1]
    umlsl v1.2d, v2.2s, v3.s[2]
    umlsl2 v1.4s, v2.8h, v3.h[3]
    umull v1.4s, v2.4h, v3.h[1]
    umull v1.2d, v2.2s, v3.s[2]
    umull2 v1.4s, v2.8h, v3.h[3]
    fmulx v1.2s, v2.2s, v3.s[2]
    fmulx v1.2d, v2.2d, v3.d[0]
.size simd_vector_x_index, . - simd_vector_x_index

conv_float_fixed:
    scvtf s1, w2, #3
    scvtf d1, w2, #3
    scvtf s1, x2, #3
    scvtf d1, x2, #3
    ucvtf s1, w2, #3
    ucvtf d1, w2, #3
    ucvtf s1, x2, #3
    ucvtf d1, x2, #3
    fcvtzs w1, s2, #3
    fcvtzs w1, d2, #3
    fcvtzs x1, s2, #3
    fcvtzs x1, d2, #3
    fcvtzu w1, s2, #3
    fcvtzu w1, d2, #3
    fcvtzu x1, s2, #3
    fcvtzu x1, d2, #3
.size conv_float_fixed, . - conv_float_fixed

conv_float_int:
    fcvtns w1, s2
    fcvtns w1, d2
    fcvtns x1, s2
    fcvtns x1, d2
    scvtf s1, w2
    scvtf d1, w2
    scvtf s1, x2
    scvtf d1, x2
    ucvtf s1, w2
    ucvtf d1, w2
    ucvtf s1, x2
    ucvtf d1, x2
    fcvtas w1, s2
    fcvtas w1, d2
    fcvtas x1, s2
    fcvtas x1, d2
    fcvtau w1, s2
    fcvtau w1, d2
    fcvtau x1, s2
    fcvtau x1, d2
    fmov s1, w2
    fmov w1, s2
    fmov d1, x2
    fmov x1, d2
    fmov v1.d[1], x2
    fmov x2, v1.d[1]
    fcvtps w1, s2
    fcvtps w1, d2
    fcvtps x1, s2
    fcvtps x1, d2
    fcvtpu w1, s2
    fcvtpu w1, d2
    fcvtpu x1, s2
    fcvtpu x1, d2
    fcvtms w1, s2
    fcvtms w1, d2
    fcvtms x1, s2
    fcvtms x1, d2
    fcvtmu w1, s2
    fcvtmu w1, d2
    fcvtmu x1, s2
    fcvtmu x1, d2
    fcvtzs w1, s2
    fcvtzs w1, d2
    fcvtzs x1, s2
    fcvtzs x1, d2
    fcvtzu w1, s2
    fcvtzu w1, d2
    fcvtzu x1, s2
    fcvtzu x1, d2
    fcvtns w1, s2
    fcvtns w1, d2
    fcvtns x1, s2
    fcvtns x1, d2
    fcvtnu w1, s2
    fcvtnu w1, d2
    fcvtnu x1, s2
    fcvtnu x1, d2
.size conv_float_int, . - conv_float_int

float_1src:
    fmov s1, s2
    fmov d1, d2
    fabs s1, s2
    fabs d1, d2
    fneg s1, s2
    fneg d1, d2
    fsqrt s1, s2
    fsqrt d1, d2
    fcvt h1, s2
    fcvt h1, d2
    fcvt s1, h2
    fcvt s1, d2
    fcvt d1, h2
    fcvt d1, s2
    frintn s1, s2
    frintn d1, d2
    frintp s1, s2
    frintp d1, d2
    frintm s1, s2
    frintm d1, d2
    frintz s1, s2
    frintz d1, d2
    frinta s1, s2
    frinta d1, d2
    frintx s1, s2
    frintx d1, d2
    frinti s1, s2
    frinti d1, d2
.size float_1src, . - float_1src

float_cmp:
    fcmp h1, h2
    fcmp s1, s2
    fcmp d1, d2
    fcmp h1, #0.0
    fcmp s1, #0.0
    fcmp d1, #0.0
    fcmpe h1, h2
    fcmpe s1, s2
    fcmpe d1, d2
    fcmpe h1, #0.0
    fcmpe s1, #0.0
    fcmpe d1, #0.0
.size float_cmp, . - float_cmp

float_imm:
    fmov h1, #4.0
    fmov s1, #0.25
    fmov d1, #8.0
.size float_imm, . - float_imm

float_ccmp:
    fccmp h1, h2, #0x1, eq
    fccmp s1, s2, #0x2, ne
    fccmp d1, d2, #0x3, ge
    fccmpe h1, h2, #0x4, lt
    fccmpe s1, s2, #0x5, hi
    fccmpe d1, d2, #0x6, hs
.size float_ccmp, . - float_ccmp

float_2src:
    fmul h1, h2, h3
    fmul s1, s2, s3
    fmul d1, d2, d3
    fdiv h1, h2, h3
    fdiv s1, s2, s3
    fdiv d1, d2, d3
    fadd h1, h2, h3
    fadd s1, s2, s3
    fadd d1, d2, d3
    fsub h1, h2, h3
    fsub s1, s2, s3
    fsub d1, d2, d3
    fmax h1, h2, h3
    fmax s1, s2, s3
    fmax d1, d2, d3
    fmin h1, h2, h3
    fmin s1, s2, s3
    fmin d1, d2, d3
    fmaxnm h1, h2, h3
    fmaxnm s1, s2, s3
    fmaxnm d1, d2, d3
    fminnm h1, h2, h3
    fminnm s1, s2, s3
    fminnm d1, d2, d3
    fnmul h1, h2, h3
    fnmul s1, s2, s3
    fnmul d1, d2, d3
.size float_2src, . - float_2src

float_csel:
    fcsel h1, h2, h3, ne
    fcsel s1, s2, s3, eq
    fcsel d1, d2, d3, gt
.size float_csel, . - float_csel

float_3src:
    fmadd h1, h2, h3, h4
    fmadd s1, s2, s3, s4
    fmadd d1, d2, d3, d4
    fmsub h1, h2, h3, h4
    fmsub s1, s2, s3, s4
    fmsub d1, d2, d3, d4
    fnmadd h1, h2, h3, h4
    fnmadd s1, s2, s3, s4
    fnmadd d1, d2, d3, d4
    fnmsub h1, h2, h3, h4
    fnmsub s1, s2, s3, s4
    fnmsub d1, d2, d3, d4
.size float_3src, . - float_3src
