; Copyright (c) 2015 Jeremy Dilatush
; All rights reserved.
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
; 
; THIS SOFTWARE IS PROVIDED BY JEREMY DILATUSH AND CONTRIBUTORS
; ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL JEREMY DILATUSH OR CONTRIBUTORS
; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.

; reload.asm - a "re loader" to load onto the VTJ-1 "hardware" and then
; use to load new software.  Load it into text (video) memory at $8000.
; Takes a number of one-byte commands:
;   0-15 - load value into low half of "staging byte"
;   16-31 - load value into upper half of "staging byte" and write
;           the staging byte to memory; increment address
;   32-47 - shift address left 4 bits; load value into low quarter
;   150 - echo the current address in hex (4 hex digits)
;           Doesn't use normal hexadecimal.
;           Instead, uses characters @-O (64-79)
;   200 - compute a 16 bit check code from the currently addressed page
;         and store it in the address
;           Check code function:
;               2 bytes state, start as zero
;               add the next byte, ones complement
;               rotate bits left
;   250 - increment the "done counter"; when it gets to 256, jumps via
;       the reset vector
; Uses a few bytes of zero page:
;   $d8 - staging byte
;   $d9 - done counter
;   $da-$db - address
;   $dc-$dd - timing cycle counter, just used for updating LEDs
;   $de-$df - used for check code computation
; On the TX side, doesn't do much, but:
;   + hex digits upon request

;; ;; ;; ;; Basic setup

    cpu 6502
    code
* = $8000

;; ;; ;; ;; Constants; mostly memory addresses

; Basic system info
LEDOUT = $b280 ; write LED state
SYSSFC = $b282 ; system special features control
    SYSSFC_BEEP = 15 ; code to start a beep
    SYSSFC_NOBEEP = 25 ; code to stop a beep
    SYSSFC_WRITE = 10 ; make the "rom" writeable
    SYSSFC_NOWRITE = 20 ; make the "rom" read-only again
SER_BASE = $b800 ; base of serial port registers
    SER_TX = $1 ; register to transmit a byte
    SER_RX = $2 ; register to receive a byte
    SER_ST = $3 ; register to query device status
    SER_TA = $4 ; transmit control byte A
    SER_TB = $5 ; transmit control byte B
    SER_RA = $6 ; receive control byte A
    SER_RB = $7 ; receive control byte B
ICTL_PEA0 = $b040 ; interrupt control: pending slot 0-7 alpha IRQs
ICTL_PEA1 = $b041 ; interrupt control: pending slot 8-15 alpha IRQs
ICTL_PEB0 = $b048 ; interrupt control: pending slot 0-7 beta IRQs
ICTL_PEB1 = $b049 ; interrupt control: pending slot 8-15 beta IRQs

;; ;; ;; ;; Memory addresses

sys_reset = $fffc ; (2 bytes) system reset vector

staging = $d8 ; byte value being read in
donectr = $d9 ; count '250' bytes received, to indicate doneness
address = $da ; (2 bytes) address to write in memory
timectr = $dc ; (2 bytes) count timing cycles before updating LEDs
checkacc = $de ; (2 bytes) used for check code computation

;; ;; ;; ;; Code

start = *
    ; disable interrupts, we won't use them
    sei

    ; initialize some of our state
    inx
    stx donectr
    ; falls through to load_address, uselessly but harmlessly

    ; load_address: shift 4 bits into the address
load_address = *
    lda address+1 ; shift the high byte first
    asl a
    asl a
    asl a
    asl a
    sta address+1
    lda address ; now shift from the low to the high byte
    and #240
    lsr a
    lsr a
    lsr a
    lsr a
    ora address+1
    sta address+1
    lda address ; and within the low byte
    asl a
    asl a
    asl a
    asl a
    sta address
    txa ; get the byte value which was received
    and #15 ; extract the low 4 bits
    ora address ; and put them into address
    sta address
    ; falls through to main_loop

    ; main loop: update LEDs; receive bytes; and handle them
main_loop = *
    ; count the times through this loop, to updated the LEDs
    inc timectr
    bne timectr_inced
    inc timectr+1
    bne timectr_inced
    inc LEDOUT ; update the LEDs
timectr_inced = *
    ; has a byte been received?
    lda ICTL_PEA1 ; "alpha" IRQ lines from slots 8-15
    ror a ; get bit 0 into carry: "alpha" IRQ of slot 8
    bcc main_loop ; not ready
    ; read the byte
    lda SER_BASE+SER_ST ; serial status byte
    ldx SER_BASE+SER_RX ; byte received
    stx SER_BASE+SER_RX ; take that byte out of the input FIFO
    ror a ; get bit 0 into carry: indicates exception vs data
    bcs main_loop ; exception, not byte: ignore
    txa ; get the byte we received, into the accumulator
    ; look at the byte & act on its contents
    cmp #150 ; is it the "echo address" command?
    beq echo_address
    cmp #200 ; is it the "check code" command?
    beq check_code
    cmp #250 ; is it the "done counter increment" command?
    beq done_ctr_inc
    and #240 ; look at top 4 bits of other commands
    beq load_low_half ; for a "load low half of staging byte" command
    cmp #16 ; for a "load high half and write staging byte" command
    beq load_staged_byte
    cmp #32 ; for a "load 4 bits of address" command
    beq load_address
    jmp main_loop

    ; echo_address: echo the current address to the terminal in simplified hex.
echo_address = *
    ; first byte
    lda address+1 ; first 2 hex digits
    jsr txhex
    lda address
    jsr txhex ; last 2 hex digits
    jmp main_loop ; done

    ; txhex: transmit two "hex" digits
txhex = *
    tay ; save the byte value for later
    lsr a ; extract the upper 4 bits
    lsr a
    lsr a
    lsr a
    ora #64 ; convert to a character
    jsr txchar ; transmit that character
    tya ; get the byte value back
    and #15 ; extract the lower 4 bits
    ora #64 ; convert to a character
    jmp txchar ; transmit that character & return

    ; txchar: transmit a byte
txchar = *
    tax ; stash the byte value for later
txchar__loop = *
    lda ICTL_PEB1 ; check the beta IRQ lines of slots 8-15
    ror a ; get bit 0 into carry, indicating port 8 is ready for write
    bcc txchar__loop ; not ready
    stx SER_BASE+SER_TX ; write the byte to port 8
    rts ; done

    ; check_code: Compute a check code for the page addressed by 'address'
    ; and store it in 'address'.
check_code = *
    lda #0 ; initialize the buffer used for check code computation
    sta checkacc
    sta checkacc+1
    tay ; pointer within the page
    tax ; just a convenient zero value to have
check_code__loop = *
    lda (address),y ; get one byte
    ; add it into checkacc (16 bit little endian ones complement)
    clc
    adc checkacc
    sta checkacc
    txa ; zero
    adc checkacc+1
    sta checkacc+1
    txa ; zero
    adc checkacc ; this makes it ones complement
    ; now rotate checkacc left one bit
    asl a
    sta checkacc
    rol checkacc+1
    txa ; zero
    adc checkacc
    sta checkacc
    ; repeat, until done with a page
    iny
    bne check_code__loop
    ; done; now copy the result to 'address'
    lda checkacc
    sta address
    lda checkacc+1
    sta address+1
    jmp main_loop ; done

    ; done_ctr_inc: Increment the 'done counter' and when it wraps, jump
    ; via reset vector
done_ctr_inc = *
    inc donectr
    bne done
    jmp main_loop ; done with the command, but not the program
done = *
    ; done with the program
    lda #SYSSFC_NOWRITE ; make the "rom" read-only again
    sta SYSSFC
    jmp (sys_reset)

    ; load_low_half: load 4 bits into staging byte
load_low_half = *
    txa ; get that byte value which was received
    and #15 ; extract the low 4 bits
    sta staging ; store in staging byte
    jmp main_loop ; done

    ; load_staged_byte: load 4 bits into staging byte & write it
load_staged_byte = *
    txa ; get that byte value which was received
    and #15 ; extract the low 4 bits
    asl a ; make them the high 4 bits
    asl a
    asl a
    asl a
    ora staging ; combine with staging byte
    ldy #0 ; and write to memory pointed to by 'address'
    sta (address),y
    inc address ; now increment 'address'
    bne load_staged_byte__idone
    inc address+1
load_staged_byte__idone = *
    jmp main_loop ; done

END_OF_CODE = *
