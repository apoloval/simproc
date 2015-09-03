;
; SimProc Examples
; Copyright (c) 2015 Alvaro Polo
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;
; This example shows how to use both input and output by reading a name
; from SPM stdin and writing it to stdout.
;
; In order to assemble, run `./target/release/spasm examples/hello-dude.asm`
; from the project root directory.
;
; In order to execute, run `./target/release/spm examples/hello-dude.asm.bin`
; from the project root directory.
;

        .org 0x0000

        ; setup the stack
        ld a0, 0xf000
        ldsp a0

        ld a1, hello
        rcall print
        rcall scan
        rcall print
        ld a1, bye
        rcall print

        halt

; -------------------------------------------------------------------------- ;
; Print a character string into SPM output
; Input a1: the address where the character string is allocated
; -------------------------------------------------------------------------- ;
print:
        ld r0, a1
        and r0, r0
        jz _print_done
        out 0x10, r0
        inc a1
        rjmp print
_print_done:
        ret

; -------------------------------------------------------------------------- ;
; Scan a character string from SPM input
; Output a1: the address where character string is allocated
; -------------------------------------------------------------------------- ;
scan:
        ld a1, 0x8000
        ldi r1, 0x0a
_scan_char:
        in r0, 0x10
        and r0, r0
        jz _scan_char
        st a1, r0
        inc a1
        sub r0, r1
        jnz _scan_char

        dec a1
        xor r0, r0
        st a1, r0
        ld a1, 0x8000
        ret

hello:
        .db "Welcome to SPM computer.\n"
        .db "Please type your name: ", 0
bye:
        .db ", thank you very much for using SPM!\n", 0
