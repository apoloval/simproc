# What's SimProc

[![Build Status](https://travis-ci.org/apoloval/simproc.svg?branch=master)](https://travis-ci.org/apoloval/simproc)

SimProc is awesome, mainly because it doesn't exist at all.

SimProc means _Simple Processor_ or _Simulated Processor_. As you wish.
SimProc is a 8-bits CPU that only exists on the paper. It was never
manufactured. Nor even designed. There are no schematic docs. It has
been fully conceived to create virtual systems that won't ever see the
light of the day. SimProc is only a set of documented CPU instructions,
with their binary coding & execution timing specifications. That's all.

On this repository, you will find emulator & dev tools to write and
execute software for SimProc.

# Why SimProc

Fun.

Did you ever dream to design your own 8-bits microcomputer? I did. Many
times. This is one step forward.

It also allows me to learn one of the most interesting programming languages
we have in sight: [Rust](http://rust-lang.org/).

# Build

Go to [Rust website](http://rust-lang.org/), download and install the latest
Rust distribution. Then, from your working copy execute:

```
cargo build --release
```

This will download all dependencies and build the binaries in `target/release`
subdirectory.

# Usage

SimProc may be used as a library to build your own virtual computer machine. It
also provides some tools useful to develop and debug SP-80 code. You will find
them in the `target/release` directory after a successful build.

* `spasm` is the SimProc assembler. You can use it to assemble your source code
into binary machine code. Execute `spasm --help` to know how to use it.

* `spm` is the SimProc Machine. It is a very simple machine with SP-80 CPU that
maps stdin and stdout to the IO port 0x10. In other words, you can write SP-80
code that reads and writes from and to your terminal using `in` and `out`
instructions.
