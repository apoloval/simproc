# What's SimProc

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
cargo build
```

This will download all dependencies and build the binaries in `target/` 
subdirectory. 
