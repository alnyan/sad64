use std::fmt;

macro_rules! system_regs {
    (
        $dir:ident,
        $(
            $op0:pat, $op1:pat, $CRn:pat, $CRm:pat, $op2:pat $(if $if_expr:expr)?
                => $name:ident $( ($param:ident: $ty:ty) )? ;
        )+
    ) => {
        #[derive(Debug, Clone, Copy)]
        pub enum SystemReg {
            $($name $(($ty))?,)+
            Other {
                op0: u8,
                op1: u8,
                CRn: u8,
                CRm: u8,
                op2: u8,
            }
        }

        impl SystemReg {
            pub fn decode($dir: DecodeDirection, op0: u8, op1: u8, CRn: u8, CRm: u8, op2: u8) -> Self {
                match (op0, op1, CRn, CRm, op2) {
                    $( ($op0, $op1, $CRn, $CRm, $op2) $(if $if_expr)? => Self::$name $(($param))?, )+
                    _ => todo!("{op0:02b} {op1:03b} {CRn:04b} {CRm:04b} {op2:03b}"),
                }
            }
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DecodeDirection {
    Msr,
    Mrs,
}

system_regs!(
    direction,
    // Special purpose registers
    0b11, 0b000, 0b0100, 0b0010, 0b010 => currentel;
    0b11, 0b011, 0b0100, 0b0010, 0b001 => daif;
    0b11, 0b011, 0b0100, 0b0101, 0b001 => dlr_el0;
    0b11, 0b011, 0b0100, 0b0101, 0b000 => dspsr_el0;
    0b11, 0b000, 0b0100, 0b0000, 0b001 => elr_el1;
    0b11, 0b100, 0b0100, 0b0000, 0b001 => elr_el2;
    0b11, 0b110, 0b0100, 0b0000, 0b001 => elr_el3;
    0b11, 0b011, 0b0100, 0b0100, 0b000 => fpcr;
    0b11, 0b011, 0b0100, 0b0100, 0b001 => fpsr;
    0b11, 0b011, 0b0100, 0b0010, 0b000 => nzcv;
    0b11, 0b000, 0b0100, 0b0001, 0b000 => sp_el0;
    0b11, 0b100, 0b0100, 0b0001, 0b000 => sp_el1;
    0b11, 0b110, 0b0100, 0b0001, 0b000 => sp_el2;
    0b11, 0b000, 0b0100, 0b0010, 0b000 => spsel;
    0b11, 0b100, 0b0100, 0b0011, 0b001 => spsr_abt;
    0b11, 0b000, 0b0100, 0b0000, 0b000 => spsr_el1;
    0b11, 0b100, 0b0100, 0b0000, 0b000 => spsr_el2;
    0b11, 0b110, 0b0100, 0b0000, 0b000 => spsr_el3;
    0b11, 0b100, 0b0100, 0b0011, 0b011 => spsr_fiq;
    0b11, 0b100, 0b0100, 0b0011, 0b000 => spsr_irq;
    0b11, 0b100, 0b0100, 0b0011, 0b010 => spsr_und;

    // General system control registers
    0b11, 0b000, 0b0001, 0b0000, 0b001 => actlr_el1;
    0b11, 0b100, 0b0001, 0b0000, 0b001 => actlr_el2;
    0b11, 0b110, 0b0001, 0b0000, 0b001 => actlr_el3;

    0b11, 0b000, 0b0101, 0b0001, 0b000 => afsr0_el1;
    0b11, 0b100, 0b0101, 0b0001, 0b000 => afsr0_el2;
    0b11, 0b110, 0b0101, 0b0001, 0b000 => afsr0_el3;

    0b11, 0b000, 0b0101, 0b0001, 0b001 => afsr1_el1;
    0b11, 0b100, 0b0101, 0b0001, 0b001 => afsr1_el2;
    0b11, 0b110, 0b0101, 0b0001, 0b001 => afsr1_el3;

    0b11, 0b001, 0b0000, 0b0000, 0b111 => aidr_el1;

    0b11, 0b000, 0b1010, 0b0011, 0b000 => amair_el1;
    0b11, 0b100, 0b1010, 0b0011, 0b000 => amair_el2;
    0b11, 0b110, 0b1010, 0b0011, 0b000 => amair_el3;

    0b11, 0b001, 0b0000, 0b0000, 0b000 => ccsidr_el1;
    0b11, 0b001, 0b0000, 0b0000, 0b001 => clidr_el1;

    0b11, 0b000, 0b1101, 0b0000, 0b001 => contextidr_el1;

    0b11, 0b000, 0b0001, 0b0000, 0b010 => cpacr_el1;
    0b11, 0b100, 0b0001, 0b0001, 0b010 => cptr_el2;
    0b11, 0b110, 0b0001, 0b0001, 0b010 => cptr_el3;

    0b11, 0b010, 0b0000, 0b0000, 0b000 => csselr_el1;

    0b11, 0b011, 0b0000, 0b0000, 0b001 => ctr_el0;

    0b11, 0b100, 0b0011, 0b0000, 0b000 => dacr32_el2;

    0b11, 0b011, 0b0000, 0b0000, 0b111 => dczid_el0;

    0b11, 0b000, 0b0101, 0b0010, 0b000 => esr_el1;
    0b11, 0b100, 0b0101, 0b0010, 0b000 => esr_el2;
    0b11, 0b110, 0b0101, 0b0010, 0b000 => esr_el3;

    0b11, 0b000, 0b0110, 0b0000, 0b000 => far_el1;
    0b11, 0b100, 0b0110, 0b0000, 0b000 => far_el2;
    0b11, 0b110, 0b0110, 0b0000, 0b000 => far_el3;

    0b11, 0b100, 0b0101, 0b0011, 0b000 => fpexc32_el2;

    0b11, 0b100, 0b0001, 0b0001, 0b111 => hacr_el2;
    0b11, 0b100, 0b0001, 0b0001, 0b000 => hcr_el2;
    0b11, 0b100, 0b0110, 0b0000, 0b100 => hpfar_el2;
    0b11, 0b100, 0b0001, 0b0001, 0b011 => hstr_el2;

    0b11, 0b000, 0b0000, 0b0101, 0b100 => id_aa64afr0_el1;
    0b11, 0b000, 0b0000, 0b0101, 0b101 => id_aa64afr1_el1;
    0b11, 0b000, 0b0000, 0b0101, 0b000 => id_aa64dfr0_el1;
    0b11, 0b000, 0b0000, 0b0101, 0b001 => id_aa64dfr1_el1;
    0b11, 0b000, 0b0000, 0b0110, 0b000 => id_aa64isar0_el1;
    0b11, 0b000, 0b0000, 0b0110, 0b001 => id_aa64isar1_el1;
    0b11, 0b000, 0b0000, 0b0111, 0b000 => id_aa64mmfr0_el1;
    0b11, 0b000, 0b0000, 0b0111, 0b001 => id_aa64mmfr1_el1;
    0b11, 0b000, 0b0000, 0b0100, 0b000 => id_aa64pfr0_el1;
    0b11, 0b000, 0b0000, 0b0100, 0b001 => id_aa64pfr1_el1;
    0b11, 0b000, 0b0000, 0b0001, 0b011 => id_afr0_el1;
    0b11, 0b000, 0b0000, 0b0001, 0b010 => id_dfr0_el1;
    0b11, 0b000, 0b0000, 0b0010, 0b000 => id_isar0_el1;
    0b11, 0b000, 0b0000, 0b0010, 0b001 => id_isar1_el1;
    0b11, 0b000, 0b0000, 0b0010, 0b010 => id_isar2_el1;
    0b11, 0b000, 0b0000, 0b0010, 0b011 => id_isar3_el1;
    0b11, 0b000, 0b0000, 0b0010, 0b100 => id_isar4_el1;
    0b11, 0b000, 0b0000, 0b0010, 0b101 => id_isar5_el1;
    0b11, 0b000, 0b0000, 0b0001, 0b100 => id_mmfr0_el1;
    0b11, 0b000, 0b0000, 0b0001, 0b101 => id_mmfr1_el1;
    0b11, 0b000, 0b0000, 0b0001, 0b110 => id_mmfr2_el1;
    0b11, 0b000, 0b0000, 0b0001, 0b111 => id_mmfr3_el1;
    0b11, 0b000, 0b0000, 0b0010, 0b110 => id_mmfr4_el1;
    0b11, 0b000, 0b0000, 0b0001, 0b000 => id_pfr0_el1;
    0b11, 0b000, 0b0000, 0b0001, 0b001 => id_pfr1_el1;

    0b11, 0b100, 0b0101, 0b0000, 0b001 => ifsr32_el2;

    0b11, 0b000, 0b1100, 0b0001, 0b000 => isr_el1;

    0b11, 0b000, 0b1010, 0b0010, 0b000 => mair_el1;
    0b11, 0b100, 0b1010, 0b0010, 0b000 => mair_el2;
    0b11, 0b110, 0b1010, 0b0010, 0b000 => mair_el3;

    0b11, 0b000, 0b0000, 0b0000, 0b000 => midr_el1;
    0b11, 0b000, 0b0000, 0b0000, 0b101 => mpidr_el1;

    0b11, 0b000, 0b0000, 0b0011, 0b000 => mvfr0_el1;
    0b11, 0b000, 0b0000, 0b0011, 0b001 => mvfr1_el1;
    0b11, 0b000, 0b0000, 0b0011, 0b010 => mvfr2_el1;

    0b11, 0b000, 0b0111, 0b0100, 0b000 => par_el1;

    0b11, 0b000, 0b0000, 0b0000, 0b110 => revidr_el1;

    0b11, 0b000, 0b1100, 0b0000, 0b010 => rmr_el1;
    0b11, 0b100, 0b1100, 0b0000, 0b010 => rmr_el2;
    0b11, 0b110, 0b1100, 0b0000, 0b010 => rmr_el3;

    0b11, 0b000, 0b1100, 0b0000, 0b001 => rvbar_el1;
    0b11, 0b100, 0b1100, 0b0000, 0b001 => rvbar_el2;
    0b11, 0b110, 0b1100, 0b0000, 0b001 => rvbar_el3;

    0b11, 0b110, 0b0001, 0b0001, 0b000 => scr_el3;

    0b11, 0b000, 0b0001, 0b0000, 0b000 => sctlr_el1;
    0b11, 0b100, 0b0001, 0b0000, 0b000 => sctlr_el2;
    0b11, 0b110, 0b0001, 0b0000, 0b000 => sctlr_el3;

    0b11, 0b000, 0b0010, 0b0000, 0b010 => tcr_el1;
    0b11, 0b100, 0b0010, 0b0000, 0b010 => tcr_el2;
    0b11, 0b110, 0b0010, 0b0000, 0b010 => tcr_el3;

    0b11, 0b011, 0b1101, 0b0000, 0b010 => tpidr_el0;
    0b11, 0b000, 0b1101, 0b0000, 0b100 => tpidr_el1;
    0b11, 0b100, 0b1101, 0b0000, 0b010 => tpidr_el2;
    0b11, 0b110, 0b1101, 0b0000, 0b010 => tpidr_el3;
    0b11, 0b011, 0b1101, 0b0000, 0b011 => tpidrro_el0;

    0b11, 0b000, 0b0010, 0b0000, 0b000 => ttbr0_el1;
    0b11, 0b100, 0b0010, 0b0000, 0b000 => ttbr0_el2;
    0b11, 0b110, 0b0010, 0b0000, 0b000 => ttbr0_el3;
    0b11, 0b000, 0b0010, 0b0000, 0b001 => ttbr1_el1;

    0b11, 0b000, 0b1100, 0b0000, 0b000 => vbar_el1;
    0b11, 0b100, 0b1100, 0b0000, 0b000 => vbar_el2;
    0b11, 0b110, 0b1100, 0b0000, 0b000 => vbar_el3;

    0b11, 0b100, 0b0000, 0b0000, 0b101 => vmpidr_el2;
    0b11, 0b100, 0b0000, 0b0000, 0b000 => vpidr_el2;
    0b11, 0b100, 0b0010, 0b0001, 0b010 => vtcr_el2;
    0b11, 0b100, 0b0010, 0b0001, 0b000 => vttbr_el2;

    // Debug registers
    0b10, 0b000, 0b0111, 0b1110, 0b110 => dbgauthstatus_el1;
    0b10, 0b000, 0b0000, n,      0b101 => dbgbcr_n_el1(n: u8);
    0b10, 0b000, 0b0000, n,      0b100 => dbgbvr_n_el1(n: u8);
    0b10, 0b000, 0b0111, 0b1001, 0b110 => dbgclaimclr_el1;
    0b10, 0b000, 0b0111, 0b1000, 0b110 => dbgclaimset_el1;
    0b10, 0b011, 0b0000, 0b0100, 0b000 => dbgdtr_el0;
    0b10, 0b011, 0b0000, 0b0101, 0b000 if direction == DecodeDirection::Mrs => dbgdtrrx_el0;
    0b10, 0b011, 0b0000, 0b0101, 0b000 if direction == DecodeDirection::Msr => dbgdtrtx_el0;
    0b10, 0b000, 0b0001, 0b0100, 0b100 => dbgprcr_el1;
    0b10, 0b100, 0b0000, 0b0111, 0b000 => dbgvcr32_el2;
    0b10, 0b000, 0b0000, n,      0b111 => dbgwcr_n_el1(n: u8);
    0b10, 0b000, 0b0000, n,      0b110 => dbgwvr_n_el1(n: u8);
    0b10, 0b000, 0b0000, 0b0010, 0b000 => mdccint_el1;
    0b10, 0b011, 0b0000, 0b0001, 0b000 => mdccsr_el0;
    0b11, 0b100, 0b0001, 0b0001, 0b001 => mdcr_el2;
    0b11, 0b110, 0b0001, 0b0011, 0b001 => mdcr_el3;
    0b10, 0b000, 0b0001, 0b0000, 0b000 => mdrar_el1;
    0b10, 0b000, 0b0000, 0b0010, 0b010 => mdscr_el1;
    0b10, 0b000, 0b0001, 0b0011, 0b100 => osdlr_el1;
    0b10, 0b000, 0b0000, 0b0000, 0b010 => osdtrrx_el1;
    0b10, 0b000, 0b0000, 0b0011, 0b010 => osdtrtx_el1;
    0b10, 0b000, 0b0000, 0b0110, 0b010 => oseccr_el1;
    0b10, 0b000, 0b0001, 0b0000, 0b100 => oslar_el1;
    0b10, 0b000, 0b0001, 0b0001, 0b100 => oslsr_el1;
    0b11, 0b110, 0b0001, 0b0001, 0b001 => sder32_el3;

    // Generic timer registers
    0b11, 0b011, 0b1110, 0b0000, 0b000 => cntfrq_el0;
    0b11, 0b100, 0b1110, 0b0001, 0b000 => cnthctl_el2;
    0b11, 0b100, 0b1110, 0b0010, 0b001 => cntp_ctl_el2;
    0b11, 0b100, 0b1110, 0b0010, 0b010 => cntp_cval_el2;
    0b11, 0b100, 0b1110, 0b0010, 0b000 => cntp_tval_el2;
    0b11, 0b000, 0b1110, 0b0001, 0b000 => cntkctl_el1;
    0b11, 0b011, 0b1110, 0b0010, 0b001 => cntp_ctl_el0;
    0b11, 0b011, 0b1110, 0b0010, 0b010 => cntp_cval_el0;
    0b11, 0b011, 0b1110, 0b0010, 0b000 => cntp_tval_el0;
    0b11, 0b011, 0b1110, 0b0000, 0b001 => cntpct_el0;
    0b11, 0b111, 0b1110, 0b0010, 0b001 => cntps_ctl_el1;
    0b11, 0b111, 0b1110, 0b0010, 0b010 => cntps_cval_el1;
    0b11, 0b111, 0b1110, 0b0010, 0b000 => cntps_tval_el1;
    0b11, 0b011, 0b1110, 0b0011, 0b001 => cntv_ctl_el0;
    0b11, 0b011, 0b1110, 0b0011, 0b010 => cntv_cval_el0;
    0b11, 0b011, 0b1110, 0b0011, 0b000 => cntv_tval_el0;
    0b11, 0b011, 0b1110, 0b0000, 0b010 => cntvct_el0;
    0b11, 0b100, 0b1110, 0b0000, 0b011 => cntvoff_el2;
);

impl fmt::Display for SystemReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Other {
                op0,
                op1,
                CRn,
                CRm,
                op2,
            } => f.write_fmt(format_args!("S{op0}_{op1}_{CRn}_{CRm}_{op2}")),
            Self::dbgbcr_n_el1(n) => f.write_fmt(format_args!("dbrbcr{}_el1", n)),
            _ => fmt::Debug::fmt(self, f),
        }
    }
}
