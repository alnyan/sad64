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
