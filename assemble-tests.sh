#!/bin/sh

aarch64-linux-gnu-gcc -c -o bins/imm.o tests/imm.S
aarch64-linux-gnu-gcc -c -o bins/branch.o tests/branch.S
