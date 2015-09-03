;
; SimProc Examples
; Copyright (c) 2015 Alvaro Polo
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;
; This example shows how to implement the classical Hello World for
; SimProc Machine.
;
; In order to assemble, run `./target/release/spasm examples/hello.asm`
; from the project root directory.
;
; In order to execute, run `./target/release/spm examples/hello.asm.bin`
; from the project root directory.
;

        .org 0x0000

        ld a1, hello
print:
        ld r0, a1
        and r0, r0
        jz done
        out 0x10, r0
        inc a1
        rjmp print
done:
        halt

hello:
        .db "Hello World!\n", 0
