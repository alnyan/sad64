# sad64

Simple AArch64 Disassembler.

### Overview

`sad64` is a minimalist AArch64 disassembly library featuring full ARMv8-A architecture
profile's AArch64 instruction set support, with base and FP/SIMD instructions.
This crate is `#![no_std]`-compatible.

### Usage

```rust
use sad64::{Formatter, SimpleFormatter, SymbolResolver, NoopResolver, StdoutOutput};

fn main() {
    let instructions = [
        0xd10203ff,
        0xf9000be2,
        0xf9400fe8,
        0x9b0a7d29,
    ];

    let mut f = SimpleFormatter;
    let resolver = NoopResolver;

    for word in instructions {
        let insn = sad64::decode(word).unwrap();
        f.write_insn(&mut StdoutOutput, &resolver, &insn).unwrap();
        println!();
    }
}
```

Will output:

```no_run,custom
sub sp, sp, #0x80
str x2, [sp, #0x10]
ldr x8, [sp, #0x18]
mul x9, x9, x10
```
