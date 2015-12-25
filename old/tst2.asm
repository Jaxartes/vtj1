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

; Something is wrong with the serial port, most likely the baud rate generation.
; This program sets the system timer (driven by the same brg() core) to 9600 Hz
; and uses that to drive the LEDs:
;       There are five LEDs
;       If I count timer cycles in 16 bits
;       and take the top 5 bits
;       I'd expect the full cycle to be 6.83 seconds

; Intended assembler: "crasm"

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
TIMER = $b0c0 ; timer built into the system controller on slot zero

; other constants
TIMER_SETTING = $15 ; supposed to be 9600 baud

; Addresses we use for particular things
low = $10 ; low byte of counter
high = $11 ; high byte of counter

* = ROMBASE

start = *
    ; Here's where the code will be when reset is done.

    ; disable interrupts; this program doesn't use them
    sei

    ; initialize the counter
    lda #234
    sta low
    sta high

    ; main loop: repeats indefinitely
main_loop = *

    ; initialize the timer to what's supposed to be 9600Hz; this also
    ; clears the timer interrupt
    lda #TIMER_SETTING
    sta TIMER

    ; put the counter value on the LEDs
    lda high
    ror a
    ror a
    ror a
    and #31
    sta LEDOUT

    ; increment the counter
    lda low
    clc
    adc #1
    sta low
    lda high
    adc #0
    sta high

    ; wait until the timer's interrupt line goes high
    ; it's the alpha IRQ of slot zero
wait_loop = *
    lda ICTL_PEA0
    and #1
    beq wait_loop ; hasn't happened yet
    bne main_loop ; has happened

* = $fffa
    dw start ; NMI vector (shouldn't run - no sources of NMI)
    dw start ; reset vector
    dw start ; IRQ/BRK vector (shouldn't run - no BRK, interrupts never enabled)
