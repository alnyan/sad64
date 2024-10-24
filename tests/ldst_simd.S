.section .text
ldst_simd_multi_struct:
    st1 { v1.8b }, [x2]
    st1 { v1.4h }, [x2]
    st1 { v1.2s }, [x2]
    st1 { v1.1d }, [x2]

    st1 { v2.16b }, [x2]
    st1 { v2.8h }, [x2]
    st1 { v2.4s }, [x2]
    st1 { v2.2d }, [x2]

    st1 { v1.1d, v2.1d }, [x2]
    st1 { v2.2d, v3.2d }, [x2]

    st1 { v1.1d, v2.1d, v3.1d }, [x2]
    st1 { v2.2d, v3.2d, v4.2d }, [x2]

    st1 { v1.1d, v2.1d, v3.1d, v4.1d }, [x2]
    st1 { v2.2d, v3.2d, v4.2d, v5.2d }, [x2]

    st1 { v1.8b, v2.8b }, [x2]

    st2 { v1.8b, v2.8b }, [x2]
    st2 { v1.2d, v2.2d }, [x2]

    st3 { v1.8b, v2.8b, v3.8b }, [x2]
    st3 { v1.2d, v2.2d, v3.2d }, [x2]

    st4 { v1.8b, v2.8b, v3.8b, v4.8b }, [x2]
    st4 { v1.2d, v2.2d, v3.2d, v4.2d }, [x2]

    ld1 { v1.8b }, [x2]
    ld1 { v1.1d }, [x2]
    ld1 { v1.1d, v2.1d }, [x2]
    ld1 { v1.1d, v2.1d, v3.1d }, [x2]
    ld1 { v1.1d, v2.1d, v3.1d, v4.1d }, [x2]

    ld2 { v1.8b, v2.8b }, [x2]
    ld2 { v1.2d, v2.2d }, [x2]

    ld3 { v1.8b, v2.8b, v3.8b }, [x2]
    ld3 { v1.2d, v2.2d, v3.2d }, [x2]

    ld4 { v1.8b, v2.8b, v3.8b, v4.8b }, [x2]
    ld4 { v1.2d, v2.2d, v3.2d, v4.2d }, [x2]
.size ldst_simd_multi_struct, . - ldst_simd_multi_struct

ldst_simd_multi_struct_reg_offset:
    st1 { v1.8b }, [x2], x3
    st1 { v1.4h }, [x2], x3
    st1 { v1.2s }, [x2], x3
    st1 { v1.1d }, [x2], x3

    st1 { v2.16b }, [x2], x3
    st1 { v2.8h }, [x2], x3
    st1 { v2.4s }, [x2], x3
    st1 { v2.2d }, [x2], x3

    st1 { v1.1d, v2.1d }, [x2], x3
    st1 { v2.2d, v3.2d }, [x2], x3

    st1 { v1.1d, v2.1d, v3.1d }, [x2], x3
    st1 { v2.2d, v3.2d, v4.2d }, [x2], x3

    st1 { v1.1d, v2.1d, v3.1d, v4.1d }, [x2], x3
    st1 { v2.2d, v3.2d, v4.2d, v5.2d }, [x2], x3

    st1 { v1.8b, v2.8b }, [x2], x3

    st2 { v1.8b, v2.8b }, [x2], x3
    st2 { v1.2d, v2.2d }, [x2], x3

    st3 { v1.8b, v2.8b, v3.8b }, [x2], x3
    st3 { v1.2d, v2.2d, v3.2d }, [x2], x3

    st4 { v1.8b, v2.8b, v3.8b, v4.8b }, [x2], x3
    st4 { v1.2d, v2.2d, v3.2d, v4.2d }, [x2], x3

    ld1 { v1.8b }, [x2], x3
    ld1 { v1.1d }, [x2], x3
    ld1 { v1.1d, v2.1d }, [x2], x3
    ld1 { v1.1d, v2.1d, v3.1d }, [x2], x3
    ld1 { v1.1d, v2.1d, v3.1d, v4.1d }, [x2], x3

    ld2 { v1.8b, v2.8b }, [x2], x3
    ld2 { v1.2d, v2.2d }, [x2], x3

    ld3 { v1.8b, v2.8b, v3.8b }, [x2], x3
    ld3 { v1.2d, v2.2d, v3.2d }, [x2], x3

    ld4 { v1.8b, v2.8b, v3.8b, v4.8b }, [x2], x3
    ld4 { v1.2d, v2.2d, v3.2d, v4.2d }, [x2], x3
.size ldst_simd_multi_struct_reg_offset, . - ldst_simd_multi_struct_reg_offset

ldst_simd_multi_struct_imm_offset:
    st1 { v1.8b }, [x2], #8
    st1 { v1.4h }, [x2], #8
    st1 { v1.2s }, [x2], #8
    st1 { v1.1d }, [x2], #8

    st1 { v2.16b }, [x2], #16
    st1 { v2.8h }, [x2], #16
    st1 { v2.4s }, [x2], #16
    st1 { v2.2d }, [x2], #16

    st1 { v1.1d, v2.1d }, [x2], #16
    st1 { v2.2d, v3.2d }, [x2], #32

    st1 { v1.1d, v2.1d, v3.1d }, [x2], #24
    st1 { v2.2d, v3.2d, v4.2d }, [x2], #48

    st1 { v1.1d, v2.1d, v3.1d, v4.1d }, [x2], #32
    st1 { v2.2d, v3.2d, v4.2d, v5.2d }, [x2], #64

    st1 { v1.8b, v2.8b }, [x2], #16

    st2 { v1.8b, v2.8b }, [x2], #16
    st2 { v1.2d, v2.2d }, [x2], #32

    st3 { v1.8b, v2.8b, v3.8b }, [x2], #24
    st3 { v1.2d, v2.2d, v3.2d }, [x2], #48

    st4 { v1.8b, v2.8b, v3.8b, v4.8b }, [x2], #32
    st4 { v1.2d, v2.2d, v3.2d, v4.2d }, [x2], #64

    ld1 { v1.8b }, [x2], #8
    ld1 { v1.1d }, [x2], #8
    ld1 { v1.1d, v2.1d }, [x2], #16
    ld1 { v1.1d, v2.1d, v3.1d }, [x2], #24
    ld1 { v1.1d, v2.1d, v3.1d, v4.1d }, [x2], #32

    ld2 { v1.8b, v2.8b }, [x2], #16
    ld2 { v1.2d, v2.2d }, [x2], #32

    ld3 { v1.8b, v2.8b, v3.8b }, [x2], #24
    ld3 { v1.2d, v2.2d, v3.2d }, [x2], #48

    ld4 { v1.8b, v2.8b, v3.8b, v4.8b }, [x2], #32
    ld4 { v1.2d, v2.2d, v3.2d, v4.2d }, [x2], #64
.size ldst_simd_multi_struct_imm_offset, . - ldst_simd_multi_struct_imm_offset

ldst_simd_single_struct:
    # st1 8 16 32 64
    st1 { v1.b }[0], [x0]
    st1 { v1.b }[15], [x0]

    st1 { v1.h }[0], [x0]
    st1 { v1.h }[7], [x0]

    st1 { v1.s }[0], [x0]
    st1 { v1.s }[3], [x0]

    st1 { v1.d }[0], [x0]
    st1 { v1.d }[1], [x0]

    # st2 8 16 32 64
    st2 { v1.b, v2.b }[0], [x0]
    st2 { v1.b, v2.b }[15], [x0]

    st2 { v1.h, v2.h }[0], [x0]
    st2 { v1.h, v2.h }[7], [x0]

    st2 { v1.s, v2.s }[0], [x0]
    st2 { v1.s, v2.s }[3], [x0]

    st2 { v1.d, v2.d }[0], [x0]
    st2 { v1.d, v2.d }[1], [x0]

    # st3 8 16 32 64
    st3 { v1.b, v2.b, v3.b }[0], [x0]
    st3 { v1.b, v2.b, v3.b }[15], [x0]

    st3 { v1.h, v2.h, v3.h }[0], [x0]
    st3 { v1.h, v2.h, v3.h }[7], [x0]

    st3 { v1.s, v2.s, v3.s }[0], [x0]
    st3 { v1.s, v2.s, v3.s }[3], [x0]

    st3 { v1.d, v2.d, v3.d }[0], [x0]
    st3 { v1.d, v2.d, v3.d }[1], [x0]

    # st4 8 16 32 64
    st4 { v1.b, v2.b, v3.b, v4.b }[0], [x0]
    st4 { v1.b, v2.b, v3.b, v4.b }[15], [x0]

    st4 { v1.h, v2.h, v3.h, v4.h }[0], [x0]
    st4 { v1.h, v2.h, v3.h, v4.h }[7], [x0]

    st4 { v1.s, v2.s, v3.s, v4.s }[0], [x0]
    st4 { v1.s, v2.s, v3.s, v4.s }[3], [x0]

    st4 { v1.d, v2.d, v3.d, v4.d }[0], [x0]
    st4 { v1.d, v2.d, v3.d, v4.d }[1], [x0]

    # ld1 8 16 32 64
    ld1 { v1.b }[0], [x0]
    ld1 { v1.b }[15], [x0]

    ld1 { v1.h }[0], [x0]
    ld1 { v1.h }[7], [x0]

    ld1 { v1.s }[0], [x0]
    ld1 { v1.s }[3], [x0]

    ld1 { v1.d }[0], [x0]
    ld1 { v1.d }[1], [x0]

    # ld2 8 16 32 64
    ld2 { v1.b, v2.b }[0], [x0]
    ld2 { v1.b, v2.b }[15], [x0]

    ld2 { v1.h, v2.h }[0], [x0]
    ld2 { v1.h, v2.h }[7], [x0]

    ld2 { v1.s, v2.s }[0], [x0]
    ld2 { v1.s, v2.s }[3], [x0]

    ld2 { v1.d, v2.d }[0], [x0]
    ld2 { v1.d, v2.d }[1], [x0]

    # ld3 8 16 32 64
    ld3 { v1.b, v2.b, v3.b }[0], [x0]
    ld3 { v1.b, v2.b, v3.b }[15], [x0]

    ld3 { v1.h, v2.h, v3.h }[0], [x0]
    ld3 { v1.h, v2.h, v3.h }[7], [x0]

    ld3 { v1.s, v2.s, v3.s }[0], [x0]
    ld3 { v1.s, v2.s, v3.s }[3], [x0]

    ld3 { v1.d, v2.d, v3.d }[0], [x0]
    ld3 { v1.d, v2.d, v3.d }[1], [x0]

    # ld4 8 16 32 64
    ld4 { v1.b, v2.b, v3.b, v4.b }[0], [x0]
    ld4 { v1.b, v2.b, v3.b, v4.b }[15], [x0]

    ld4 { v1.h, v2.h, v3.h, v4.h }[0], [x0]
    ld4 { v1.h, v2.h, v3.h, v4.h }[7], [x0]

    ld4 { v1.s, v2.s, v3.s, v4.s }[0], [x0]
    ld4 { v1.s, v2.s, v3.s, v4.s }[3], [x0]

    ld4 { v1.d, v2.d, v3.d, v4.d }[0], [x0]
    ld4 { v1.d, v2.d, v3.d, v4.d }[1], [x0]

    # ld1r ld2r ld3r ld4r
    ld1r { v1.8b }, [x0]
    ld1r { v1.8h }, [x0]

    ld2r { v1.8b, v2.8b }, [x0]
    ld2r { v1.8h, v2.8h }, [x0]

    ld3r { v1.8b, v2.8b, v3.8b }, [x0]
    ld3r { v1.8h, v2.8h, v3.8h }, [x0]

    ld4r { v1.8b, v2.8b, v3.8b, v4.8b }, [x0]
    ld4r { v1.8h, v2.8h, v3.8h, v4.8h }, [x0]
.size ldst_simd_single_struct, . - ldst_simd_single_struct

ldst_simd_single_struct_reg_offset:
    # st1 8 16 32 64
    st1 { v1.b }[0], [x0], x1
    st1 { v1.b }[15], [x0], x1

    st1 { v1.h }[0], [x0], x1
    st1 { v1.h }[7], [x0], x1

    st1 { v1.s }[0], [x0], x1
    st1 { v1.s }[3], [x0], x1

    st1 { v1.d }[0], [x0], x1
    st1 { v1.d }[1], [x0], x1

    # st2 8 16 32 64
    st2 { v1.b, v2.b }[0], [x0], x1
    st2 { v1.b, v2.b }[15], [x0], x1

    st2 { v1.h, v2.h }[0], [x0], x1
    st2 { v1.h, v2.h }[7], [x0], x1

    st2 { v1.s, v2.s }[0], [x0], x1
    st2 { v1.s, v2.s }[3], [x0], x1

    st2 { v1.d, v2.d }[0], [x0], x1
    st2 { v1.d, v2.d }[1], [x0], x1

    # st3 8 16 32 64
    st3 { v1.b, v2.b, v3.b }[0], [x0], x1
    st3 { v1.b, v2.b, v3.b }[15], [x0], x1

    st3 { v1.h, v2.h, v3.h }[0], [x0], x1
    st3 { v1.h, v2.h, v3.h }[7], [x0], x1

    st3 { v1.s, v2.s, v3.s }[0], [x0], x1
    st3 { v1.s, v2.s, v3.s }[3], [x0], x1

    st3 { v1.d, v2.d, v3.d }[0], [x0], x1
    st3 { v1.d, v2.d, v3.d }[1], [x0], x1

    # st4 8 16 32 64
    st4 { v1.b, v2.b, v3.b, v4.b }[0], [x0], x1
    st4 { v1.b, v2.b, v3.b, v4.b }[15], [x0], x1

    st4 { v1.h, v2.h, v3.h, v4.h }[0], [x0], x1
    st4 { v1.h, v2.h, v3.h, v4.h }[7], [x0], x1

    st4 { v1.s, v2.s, v3.s, v4.s }[0], [x0], x1
    st4 { v1.s, v2.s, v3.s, v4.s }[3], [x0], x1

    st4 { v1.d, v2.d, v3.d, v4.d }[0], [x0], x1
    st4 { v1.d, v2.d, v3.d, v4.d }[1], [x0], x1

    # ld1 8 16 32 64
    ld1 { v1.b }[0], [x0], x1
    ld1 { v1.b }[15], [x0], x1

    ld1 { v1.h }[0], [x0], x1
    ld1 { v1.h }[7], [x0], x1

    ld1 { v1.s }[0], [x0], x1
    ld1 { v1.s }[3], [x0], x1

    ld1 { v1.d }[0], [x0], x1
    ld1 { v1.d }[1], [x0], x1

    # ld2 8 16 32 64
    ld2 { v1.b, v2.b }[0], [x0], x1
    ld2 { v1.b, v2.b }[15], [x0], x1

    ld2 { v1.h, v2.h }[0], [x0], x1
    ld2 { v1.h, v2.h }[7], [x0], x1

    ld2 { v1.s, v2.s }[0], [x0], x1
    ld2 { v1.s, v2.s }[3], [x0], x1

    ld2 { v1.d, v2.d }[0], [x0], x1
    ld2 { v1.d, v2.d }[1], [x0], x1

    # ld3 8 16 32 64
    ld3 { v1.b, v2.b, v3.b }[0], [x0], x1
    ld3 { v1.b, v2.b, v3.b }[15], [x0], x1

    ld3 { v1.h, v2.h, v3.h }[0], [x0], x1
    ld3 { v1.h, v2.h, v3.h }[7], [x0], x1

    ld3 { v1.s, v2.s, v3.s }[0], [x0], x1
    ld3 { v1.s, v2.s, v3.s }[3], [x0], x1

    ld3 { v1.d, v2.d, v3.d }[0], [x0], x1
    ld3 { v1.d, v2.d, v3.d }[1], [x0], x1

    # ld4 8 16 32 64
    ld4 { v1.b, v2.b, v3.b, v4.b }[0], [x0], x1
    ld4 { v1.b, v2.b, v3.b, v4.b }[15], [x0], x1

    ld4 { v1.h, v2.h, v3.h, v4.h }[0], [x0], x1
    ld4 { v1.h, v2.h, v3.h, v4.h }[7], [x0], x1

    ld4 { v1.s, v2.s, v3.s, v4.s }[0], [x0], x1
    ld4 { v1.s, v2.s, v3.s, v4.s }[3], [x0], x1

    ld4 { v1.d, v2.d, v3.d, v4.d }[0], [x0], x1
    ld4 { v1.d, v2.d, v3.d, v4.d }[1], [x0], x1

    # ld1r ld2r ld3r ld4r
    ld1r { v1.8b }, [x0], x1
    ld1r { v1.8h }, [x0], x1

    ld2r { v1.8b, v2.8b }, [x0], x1
    ld2r { v1.8h, v2.8h }, [x0], x1

    ld3r { v1.8b, v2.8b, v3.8b }, [x0], x1
    ld3r { v1.8h, v2.8h, v3.8h }, [x0], x1

    ld4r { v1.8b, v2.8b, v3.8b, v4.8b }, [x0], x1
    ld4r { v1.8h, v2.8h, v3.8h, v4.8h }, [x0], x1
.size ldst_simd_single_struct_reg_offset, . - ldst_simd_single_struct_reg_offset

ldst_simd_single_struct_imm_offset:
    # st1 8 16 32 64
    st1 { v1.b }[0], [x0], #1
    st1 { v1.b }[15], [x0], #1

    st1 { v1.h }[0], [x0], #2
    st1 { v1.h }[7], [x0], #2

    st1 { v1.s }[0], [x0], #4
    st1 { v1.s }[3], [x0], #4

    st1 { v1.d }[0], [x0], #8
    st1 { v1.d }[1], [x0], #8

    # st2 8 16 32 64
    st2 { v1.b, v2.b }[0], [x0], #2
    st2 { v1.b, v2.b }[15], [x0], #2

    st2 { v1.h, v2.h }[0], [x0], #4
    st2 { v1.h, v2.h }[7], [x0], #4

    st2 { v1.s, v2.s }[0], [x0], #8
    st2 { v1.s, v2.s }[3], [x0], #8

    st2 { v1.d, v2.d }[0], [x0], #16
    st2 { v1.d, v2.d }[1], [x0], #16

    # st3 8 16 32 64
    st3 { v1.b, v2.b, v3.b }[0], [x0], #3
    st3 { v1.b, v2.b, v3.b }[15], [x0], #3

    st3 { v1.h, v2.h, v3.h }[0], [x0], #6
    st3 { v1.h, v2.h, v3.h }[7], [x0], #6

    st3 { v1.s, v2.s, v3.s }[0], [x0], #12
    st3 { v1.s, v2.s, v3.s }[3], [x0], #12

    st3 { v1.d, v2.d, v3.d }[0], [x0], #24
    st3 { v1.d, v2.d, v3.d }[1], [x0], #24

    # st4 8 16 32 64
    st4 { v1.b, v2.b, v3.b, v4.b }[0], [x0], #4
    st4 { v1.b, v2.b, v3.b, v4.b }[15], [x0], #4

    st4 { v1.h, v2.h, v3.h, v4.h }[0], [x0], #8
    st4 { v1.h, v2.h, v3.h, v4.h }[7], [x0], #8

    st4 { v1.s, v2.s, v3.s, v4.s }[0], [x0], #16
    st4 { v1.s, v2.s, v3.s, v4.s }[3], [x0], #16

    st4 { v1.d, v2.d, v3.d, v4.d }[0], [x0], #32
    st4 { v1.d, v2.d, v3.d, v4.d }[1], [x0], #32

    # ld1 8 16 32 64
    ld1 { v1.b }[0], [x0], #1
    ld1 { v1.b }[15], [x0], #1

    ld1 { v1.h }[0], [x0], #2
    ld1 { v1.h }[7], [x0], #2

    ld1 { v1.s }[0], [x0], #4
    ld1 { v1.s }[3], [x0], #4

    ld1 { v1.d }[0], [x0], #8
    ld1 { v1.d }[1], [x0], #8

    # ld2 8 16 32 64
    ld2 { v1.b, v2.b }[0], [x0], #2
    ld2 { v1.b, v2.b }[15], [x0], #2

    ld2 { v1.h, v2.h }[0], [x0], #4
    ld2 { v1.h, v2.h }[7], [x0], #4

    ld2 { v1.s, v2.s }[0], [x0], #8
    ld2 { v1.s, v2.s }[3], [x0], #8

    ld2 { v1.d, v2.d }[0], [x0], #16
    ld2 { v1.d, v2.d }[1], [x0], #16

    # ld3 8 16 32 64
    ld3 { v1.b, v2.b, v3.b }[0], [x0], #3
    ld3 { v1.b, v2.b, v3.b }[15], [x0], #3

    ld3 { v1.h, v2.h, v3.h }[0], [x0], #6
    ld3 { v1.h, v2.h, v3.h }[7], [x0], #6

    ld3 { v1.s, v2.s, v3.s }[0], [x0], #12
    ld3 { v1.s, v2.s, v3.s }[3], [x0], #12

    ld3 { v1.d, v2.d, v3.d }[0], [x0], #24
    ld3 { v1.d, v2.d, v3.d }[1], [x0], #24

    # ld4 8 16 32 64
    ld4 { v1.b, v2.b, v3.b, v4.b }[0], [x0], #4
    ld4 { v1.b, v2.b, v3.b, v4.b }[15], [x0], #4

    ld4 { v1.h, v2.h, v3.h, v4.h }[0], [x0], #8
    ld4 { v1.h, v2.h, v3.h, v4.h }[7], [x0], #8

    ld4 { v1.s, v2.s, v3.s, v4.s }[0], [x0], #16
    ld4 { v1.s, v2.s, v3.s, v4.s }[3], [x0], #16

    ld4 { v1.d, v2.d, v3.d, v4.d }[0], [x0], #32
    ld4 { v1.d, v2.d, v3.d, v4.d }[1], [x0], #32

    # ld1r ld2r ld3r ld4r
    ld1r { v1.8b }, [x0], #1
    ld1r { v1.8h }, [x0], #2
    ld1r { v1.4s }, [x0], #4
    ld1r { v1.2d }, [x0], #8

    ld2r { v1.8b, v2.8b }, [x0], #2
    ld2r { v1.8h, v2.8h }, [x0], #4

    ld3r { v1.8b, v2.8b, v3.8b }, [x0], #3
    ld3r { v1.8h, v2.8h, v3.8h }, [x0], #6

    ld4r { v1.8b, v2.8b, v3.8b, v4.8b }, [x0], #4
    ld4r { v1.8h, v2.8h, v3.8h, v4.8h }, [x0], #8
.size ldst_simd_single_struct_imm_offset, . - ldst_simd_single_struct_imm_offset

ldst_simd_literal:
    ldr s1, =0x12345678
    ldr d2, =0x1122334455667788
    ldr q3, =0x11112222333344445555666677778888
.size ldst_simd_literal, . - ldst_simd_literal

ldst_simd_pair:
    stnp s1, s2, [x0, #4]
    stnp d2, d3, [x0, #8]
    stnp q3, q4, [x0, #16]
    ldnp s1, s2, [x0, #4]
    ldnp d2, d3, [x0, #8]
    ldnp q3, q4, [x0, #16]

    stp s1, s2, [x0], #4
    stp d2, d3, [x0], #8
    stp q3, q4, [x0], #16
    ldp s1, s2, [x0], #4
    ldp d2, d3, [x0], #8
    ldp q3, q4, [x0], #16

    stp s1, s2, [x0, #4]
    stp d2, d3, [x0, #8]
    stp q3, q4, [x0, #16]
    ldp s1, s2, [x0, #4]
    ldp d2, d3, [x0, #8]
    ldp q3, q4, [x0, #16]

    stp s1, s2, [x0, #4]!
    stp d2, d3, [x0, #8]!
    stp q3, q4, [x0, #16]!
    ldp s1, s2, [x0, #4]!
    ldp d2, d3, [x0, #8]!
    ldp q3, q4, [x0, #16]!
.size ldst_simd_pair, . - ldst_simd_pair

ldst_simd_single:
    stur b1, [x0, #1]
    str b1, [x0, #1]
    # No pre-index
    str b1, [x0], #1
    ldur b1, [x0, #1]
    ldr b1, [x0, #1]
    # No pre-index
    ldr b1, [x0], #1

    stur h2, [x0, #2]
    str h2, [x0, #2]
    # No pre-index
    str h2, [x0], #2
    ldur h2, [x0, #2]
    ldr h2, [x0, #2]
    # No pre-index
    ldr h2, [x0], #2

    stur s3, [x0, #3]
    str s3, [x0, #4]
    str s3, [x0, #3]!
    str s3, [x0], #3
    ldur s3, [x0, #3]
    ldr s3, [x0, #4]
    ldr s3, [x0, #3]!
    ldr s3, [x0], #3

    stur d4, [x0, #4]
    str d4, [x0, #8]
    str d4, [x0, #8]!
    str d4, [x0], #8
    ldur d4, [x0, #4]
    ldr d4, [x0, #8]
    ldr d4, [x0, #8]!
    ldr d4, [x0], #8

    stur q5, [x0, #5]
    str q5, [x0, #16]
    str q5, [x0, #16]!
    str q5, [x0], #16
    ldur q5, [x0, #5]
    ldr q5, [x0, #16]
    ldr q5, [x0, #16]!
    ldr q5, [x0], #16
.size ldst_simd_single, . - ldst_simd_single

ldst_simd_single_reg_offset:
    str b1, [x0, w1, sxtw]
    str b1, [x0, w1, uxtw]
    str b1, [x0, x1, sxtx]
    ldr b1, [x0, w1, sxtw]
    ldr b1, [x0, w1, uxtw]
    ldr b1, [x0, x1, sxtx]

    str h1, [x0, w1, sxtw]
    str h1, [x0, w1, sxtw #1]
    str h1, [x0, w1, uxtw]
    str h1, [x0, w1, uxtw #1]
    str h1, [x0, x1, sxtx]
    str h1, [x0, x1, sxtx #1]
    str h1, [x0, x1, lsl #1]
    ldr h1, [x0, w1, sxtw]
    ldr h1, [x0, w1, sxtw #1]
    ldr h1, [x0, w1, uxtw]
    ldr h1, [x0, w1, uxtw #1]
    ldr h1, [x0, x1, sxtx]
    ldr h1, [x0, x1, sxtx #1]
    ldr h1, [x0, x1, lsl #1]

    str s1, [x0, w1, sxtw]
    str s1, [x0, w1, sxtw #2]
    str s1, [x0, w1, uxtw]
    str s1, [x0, w1, uxtw #2]
    str s1, [x0, x1, sxtx]
    str s1, [x0, x1, sxtx #2]
    str s1, [x0, x1, lsl #2]
    ldr s1, [x0, w1, sxtw]
    ldr s1, [x0, w1, sxtw #2]
    ldr s1, [x0, w1, uxtw]
    ldr s1, [x0, w1, uxtw #2]
    ldr s1, [x0, x1, sxtx]
    ldr s1, [x0, x1, sxtx #2]
    ldr s1, [x0, x1, lsl #2]

    str d1, [x0, w1, sxtw]
    str d1, [x0, w1, sxtw #3]
    str d1, [x0, w1, uxtw]
    str d1, [x0, w1, uxtw #3]
    str d1, [x0, x1, sxtx]
    str d1, [x0, x1, sxtx #3]
    str d1, [x0, x1, lsl #3]
    ldr d1, [x0, w1, sxtw]
    ldr d1, [x0, w1, sxtw #3]
    ldr d1, [x0, w1, uxtw]
    ldr d1, [x0, w1, uxtw #3]
    ldr d1, [x0, x1, sxtx]
    ldr d1, [x0, x1, sxtx #3]
    ldr d1, [x0, x1, lsl #3]

    str q1, [x0, w1, sxtw]
    str q1, [x0, w1, sxtw #4]
    str q1, [x0, w1, uxtw]
    str q1, [x0, w1, uxtw #4]
    str q1, [x0, x1, sxtx]
    str q1, [x0, x1, sxtx #4]
    str q1, [x0, x1, lsl #4]
    ldr q1, [x0, w1, sxtw]
    ldr q1, [x0, w1, sxtw #4]
    ldr q1, [x0, w1, uxtw]
    ldr q1, [x0, w1, uxtw #4]
    ldr q1, [x0, x1, sxtx]
    ldr q1, [x0, x1, sxtx #4]
    ldr q1, [x0, x1, lsl #4]
.size ldst_simd_single_reg_offset, . - ldst_simd_single_reg_offset
