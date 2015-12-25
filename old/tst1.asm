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

; First test program for the "VTJ-1" system running on FPGA.  See system
; documentation.  This test is quite minimal: It sets LEDs and tries to
; write characters to the serial port.  All without interrupts.

    cpu 6502
    code

; Basic system info
RAMBASE = $0000 ; start of general purpose RAM
RAMTOP = $0800 ; byte after the last one of general purpose RAM
ROMBASE = $c000 ; start of ROM
LEDOUT = $b280 ; write LED state
SER_BASE = $b800 ; base of serial port registers
    SER_INFO = $0 ; offset of serial port information byte
    SER_TX = $1 ; register to transmit a byte
    SER_RX = $2 ; register to receive a byte
    SER_ST = $3 ; register to query device status
    SER_TA = $4 ; transmit control byte A
    SER_TB = $5 ; transmit control byte B
    SER_RA = $6 ; receive control byte A
    SER_RB = $7 ; receive control byte B
    SER_LBL = $10 ; start of an ASCIIZ label
ICTL_ENA0 = $b000 ; interrupt control: enable slot 0-7 alpha IRQs
ICTL_ENA1 = $b001 ; interrupt control: enable slot 8-15 alpha IRQs
ICTL_ENB0 = $b008 ; interrupt control: enable slot 0-7 beta IRQs
ICTL_ENB1 = $b009 ; interrupt control: enable slot 8-15 beta IRQs
ICTL_PEA0 = $b040 ; interrupt control: pending slot 0-7 alpha IRQs
ICTL_PEA1 = $b041 ; interrupt control: pending slot 8-15 alpha IRQs
ICTL_PEB0 = $b048 ; interrupt control: pending slot 0-7 beta IRQs
ICTL_PEB1 = $b049 ; interrupt control: pending slot 8-15 beta IRQs

; Addresses we use for particular things
ledreg = $10 ; keep track of LED state
chareg = $11 ; keep track of output character

* = ROMBASE

start = *
    ; Here's where the code will be when reset is done.

    ; disable interrupts; this program doesn't use them
    sei

    ; Update the LED state variable, which may be retained across resets.
    ; Updated by adding 23 each time.
    lda ledreg
    clc
    adc #23
    sta ledreg

    ; And put that out on the LEDs
    and #$1f
    sta LEDOUT

    ; Set up the serial port for output.
    lda #$15 ; 9600 baud
    sta SER_BASE+SER_TA
    lda #$30 ; 8n2
    sta SER_BASE+SER_TB

char_init = *
    ; Initialize the character we'll output
    lda #33
    sta chareg

    ; Main loop: output characters
main_loop = *
    ; Check for readiness: slot 8's beta IRQ
    lda ICTL_PEB1 ; beta IRQs of slots 8-15
    and #1 ; select slot 8
    beq main_loop ; not ready

    ; It's ready; write the character
    lda chareg
    sta SER_BASE+SER_TX

    ; Pick the next character and repeat
    clc
    adc #1
    cmp #127
    sta chareg
    bne main_loop ; ok to repeat with next byte value
    beq char_init ; repeat after starting the character sequence

* = $fffa
    dw start ; NMI vector (shouldn't run - no sources of NMI)
    dw start ; reset vector
    dw start ; IRQ/BRK vector (shouldn't run - no BRK, interrupts never enabled)
