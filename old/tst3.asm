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

; For testing my 6502 based "VTJ-1" platform:
; Enumerate the sums of squares on a 115,200 baud serial port.

; Uses unpacked BCD, which is inefficient but easy.
; This program is going to be I/O bound not CPU bound anyway.

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
;;; BAUD_SETTING = $15 ; 9600 baud
BAUD_SETTING = $03 ; 115,200 baud
NUM_LEN = 10 ; maximum digits per number

; Addresses we use for particular things (non zero page)
nx = $200 ; addr of "x", in little endian unpacked BCD format
ny = $210 ; addr of "y", in little endian unpacked BCD format
nx2 = $220 ; addr of "x^2", in little endian unpacked BCD format
nx2y2 = $230 ; addr of "x^2+y^2", in little endian unpacked BCD format

; Zero page addresses
np0 = $10 ; 2 byte - pointer to 1st numeric parameter
np1 = $12 ; 2 byte - pointer to 2nd numeric parameter
np2 = $14 ; 2 byte - pointer to 3rd numeric parameter

tmp1 = $20 ; 1 byte - temporary storage used by numadd2p1

* = ROMBASE

loadparm MACRO
    ; Macro to load two bytes into a pointer
    ; Used for numeric parameters.
    ; \1 is the value
    ; \2 is where to put it
    lda #\1 & 255
    sta \2
    lda #(\1 >> 8) & 255
    sta \2+1
ENDM

setled MACRO
    pha
    lda #\1
    sta LEDOUT
    pla
ENDM

start = *
    ; Here's where the code will be when reset is done.

    ; disable interrupts; this program doesn't use them
    sei

    ; initialize serial port
    lda #BAUD_SETTING
    sta SER_BASE+SER_TA ; set the baud rate
    lda #$30
    sta SER_BASE+SER_TB ; set to 8n2 (compatible with 8n1)

    ; initialize "x" and "x^2" to zero
    loadparm nx, np0
    loadparm num_zero, np1
    jsr numcpy
    loadparm nx2, np0
    loadparm num_zero, np1
    jsr numcpy

    ; print a blank line to make the start of the output easier to analyze
    lda #13
    jsr chartx
    lda #10
    jsr chartx

loop_x = *
    ; loop that runs for each "x" value

    ; initialize "y" to zero
    loadparm ny, np0
    loadparm num_zero, np1
    jsr numcpy

    ; initialize "x^2+y^2" to "x^2"
    loadparm nx2y2, np0
    loadparm nx2, np1
    jsr numcpy

loop_y = *
    ; loop that runs for each "y" value

    ; display the x^2 + y^2 line
    loadparm nx, np0
    jsr numtx
    lda #'^'
    jsr chartx
    lda #'2'
    jsr chartx
    lda #' '
    jsr chartx
    lda #'+'
    jsr chartx
    lda #' '
    jsr chartx
    loadparm ny, np0
    jsr numtx
    lda #'^'
    jsr chartx
    lda #'2'
    jsr chartx
    lda #' '
    jsr chartx
    lda #'='
    jsr chartx
    lda #' '
    jsr chartx
    loadparm nx2y2, np0
    jsr numtx
    lda #13
    jsr chartx
    lda #10
    jsr chartx

    ; now see if y == x
    loadparm nx, np0
    loadparm ny, np1
    jsr numeq
    beq next_x ; they are: increment "x"

    ; since y != x, assume y < x; increment y
    loadparm nx2y2, np0
    loadparm ny, np1
    jsr numadd2p1 ; x2y2 += 2*y + 1
    loadparm ny, np0
    loadparm num_zero, np1
    jsr numadd2p1 ; y += 0*2 + 1, that is y++
    jmp loop_y ; and process that new value

next_x = *
    ; since y = x, increment x; and reset y to zero and go from there
    loadparm nx2, np0
    loadparm nx, np1
    jsr numadd2p1 ; x2 += 2*x + 1
    loadparm nx, np0
    loadparm num_zero, np1
    jsr numadd2p1 ; x += 0*2 + 1, that is x++
    jmp loop_x

num_zero = *
    ; numeric constant: zero
    db 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0

numadd2p1_tbl1 = *
    ; table for use in "numadd2p1" - for each of 30 input values it
    ; gives value divided by 10
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
    db 2, 2, 2, 2, 2, 2, 2, 2, 2, 2

numadd2p1_tbl2 = *
    ; table for use in "numadd2p1" - for each of 30 input values it
    ; gives value modulo 10
    db 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
    db 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
    db 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

numcpy = *
    ; copy number to number
    ; Numeric parameters:
    ;   np0 - pointer to the destination
    ;   np1 - pointer to the source
    ldy #0 ; Y will index into the numbers
    ldx #NUM_LEN ; X will count digits left to go
numcpy__loop = *
    lda (np1),y
    sta (np0),y
    iny
    dex ; one less digit to go; any left
    bne numcpy__loop ; yes: repeat
    rts ; no: return

chartx = *
    ; Take the character in the accumulator & write it out to the
    ; serial port.  Delays until it's time to transmit.  Doesn't
    ; alter any of the registers.

    ; stash the character value
    pha

    ; Wait until the serial port is ready; that's slot 8's beta IRQ.
    ; This program runs with interrupts disabled, but the interrupt controller
    ; is still working & can be polled.
chartx__loop = *
    lda ICTL_PEB1 ; beta IRQs of slots 8-15
    and #1 ; select slot 8
    beq chartx__loop ; not ready

    ; write the character now
    pla
    sta SER_BASE+SER_TX

    ; and done
    rts

numtx = *
    ; write a number out on the serial port
    ; Numeric parameters:
    ;   np0 - pointer to the number
    ; The number is stored in unpacked BCD, little endian.

    ; Search for the most significant nonzero digit
    ldy #NUM_LEN-1 ; Y will point within the number
numtx__sloop = *
    lda (np0),y ; get the digit
    bne numtx__nonz ; turns out it's not zero
    dey ; try the next smaller digit
    bne numtx__sloop ; but only if it's not the least order one;
                     ; zero is written as "0"
numtx__nonz = * 
numtx__dloop = *
    ; At this point (np0),y is the most significant nonzero digit.
    ; Write that out followed by the less significant ones.
    lda (np0),y ; get the digit
    clc
    adc #48 ; convert to ASCII
    jsr chartx ; and write it out
    dey
    bpl numtx__dloop ; if Y >= 0 it's another digit to do

    ; Y < 0: out of digits; done
    rts

numeq = *
    ; compare two numbers
    ; sets flags Z and N based on comparison
    ; Numeric parameters:
    ;   np0 - pointer to one of the numbers (left side)
    ;   np1 - pointer to the other number (right side)
    ; Start comparing digit by digit from the most significant.
    ldy #NUM_LEN-1 ; Y will point within the number
numeq__loop = *
    lda (np0),y ; get first digit of left side
    cmp (np1),y ; compare with first digit of right side
    bne numeq__done ; if they're not equal they determine the result
    ; try the next digit if there is one
    dey
    bpl numeq__loop ; if Y >= 0 there's another digit
    ; so: we ran out of digits, meaning the numbers are equal
    ; but right now the flags are set for "less than"
    iny ; since Y was 255, now Y = 0 and the Z flag will be set
numeq__done = *
    rts ; we've set the flags & we can return

numadd2p1 = *
    ; Performs the following operation on two parameters:
    ;   *np0 += *np1 * 2 + 1
    ; Uses two lookup tables to assist:
    ;   numadd2p1_tbl1 (0-29 divided by 10)
    ;   numadd2p1_tbl2 (0-29 modulo 10)
    lda #NUM_LEN
    sta tmp1 ; digits to go
    lda #1 ; initial carry value
    ldy #0 ; initial digit pointer
numadd2p1__loop = *
    ; add digits
    clc ; clear carry, and it'll stay clear
    adc (np0),y ; A = carryin + digit from np0
    adc (np1),y ; A = carryin + digit from np0 + digit from np1
    adc (np1),y ; A = carryin + digit from np0 + 2 * digit from np1
    ; use the result & the lookup tables to produce new digit and carry
    tax
    lda numadd2p1_tbl2,x
    sta (np0),y ; new digit (from remainder)
    lda numadd2p1_tbl1,x ; new carry (from quotient)
    ; go on to the next digit if there are any
    iny
    dec tmp1
    bne numadd2p1__loop ; repeat if there are digits left to do
    rts ; return if there aren't

* = $fffa
    dw start ; NMI vector (shouldn't run - no sources of NMI)
    dw start ; reset vector
    dw start ; IRQ/BRK vector (shouldn't run - no BRK, interrupts never enabled)
