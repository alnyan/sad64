#!/bin/sh

aarch64-linux-gnu-gcc -c -o bins/imm.o tests/imm.S
aarch64-linux-gnu-gcc -c -o bins/branch.o tests/branch.S
aarch64-linux-gnu-gcc -c -o bins/ldst.o tests/ldst.S
aarch64-linux-gnu-gcc -mcpu=cortex-a72 -c -o bins/dataproc.o tests/dataproc.S
