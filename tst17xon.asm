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

; Test program to get a handle on how XON/XOFF flow control works, in
; combination with the host (Linux) and the USB - serial interface (FTDI).

; Intended assembler: "crasm"

;; ;; ;; ;; Basic setup

    cpu 6502
    code
* = $c000

; Basic system info
LEDOUT = $b280 ; write LED state
SER_BASE = $b800 ; base of serial port registers
    SER_TX = $1 ; register to transmit a byte
    SER_RX = $2 ; register to receive a byte
    SER_ST = $3 ; register to query device status
    SER_TA = $4 ; transmit control byte A
    SER_TB = $5 ; transmit control byte B
    SER_RA = $6 ; receive control byte A
    SER_RB = $7 ; receive control byte B
    BAUDCODE = $03 ; code for 115.2k baud ($15 would be 9600 baud)
    TXMODE = $30 ; code for 8n2, compatible with 8n1
    RXMODE = $20 ; code for 8n1
ICTL_ENA0 = $b000 ; interrupt control: enable slot 0-7 alpha IRQs
ICTL_ENA1 = $b001 ; interrupt control: enable slot 8-15 alpha IRQs
ICTL_ENB0 = $b008 ; interrupt control: enable slot 0-7 beta IRQs
ICTL_ENB1 = $b009 ; interrupt control: enable slot 8-15 beta IRQs
ICTL_PEA0 = $b040 ; interrupt control: pending slot 0-7 alpha IRQs
ICTL_PEA1 = $b041 ; interrupt control: pending slot 8-15 alpha IRQs
ICTL_PEB0 = $b048 ; interrupt control: pending slot 0-7 beta IRQs
ICTL_PEB1 = $b049 ; interrupt control: pending slot 8-15 beta IRQs
ICTL_PRI = $b080 ; interrupt control: priority encoding

XON_CHAR = 17 ; character code for XON
XOFF_CHAR = 19 ; character code for XOFF

; Zero page addresses
ctr_rx = $00 ; 8 bytes - non-flow control bytes received (unpacked decimal)
ctr_rxon = $08 ; 8 bytes - XON bytes received
ctr_rxoff = $10 ; 8 bytes - XOFF bytes received
draw_row = $18 ; 1 byte - drawing position, row 0-38
draw_col = $19 ; 1 byte - drawing position, col 0-38
draw_blk = $1a ; 1 byte - drawing: color (0) or black (1)
addr0 = $1b ; 2 bytes - pointer to data
atmp = $1d ; 2 bytes - temporary manipulation of addresses, or other
           ; 16-bit integers, when not in interrupt mode
status_ctr = $1f ; 1 byte - count 0-255 & show status at 0
go_row = $20 ; 1 byte - cursor position to go to, row part, 0 based
go_col = $21 ; 1 byte - cursor position to go to, col part, 0 based
preserve_a = $22 ; 1 byte - preserve A while in txchar()
preserve_x = $23 ; 1 byte - preserve X while in txchar()
preserve_y = $24 ; 1 byte - preserve Y while in txchar()
prngs = $25 ; 4 bytes - pseudo random number generator state
xoffed = $29 ; 1 byte - if we've received XOFF
ctr_tx = $2a ; 8 bytes - non-flow control bytes transmitted
ctr_txon = $32 ; 8 bytes - XON bytes transmitted
ctr_txoff = $3a ; 8 bytes - XOFF bytes transmitted

; Code

start = *
    ; disable interrupts; we're not going to use them
    sei

    ; initialize serial port to 115.2 kbaud
    lda #BAUDCODE
    sta SER_BASE+SER_TA
    sta SER_BASE+SER_RA
    lda #TXMODE
    sta SER_BASE+SER_TB
    lda #RXMODE
    sta SER_BASE+SER_RB

    ; zero the entire zero page of memory
    ldx #0
    txa
start__zerozp = *
    sta 0,x
    inx
    bne start__zerozp

    ; now for the main loop (which is not the event loop)
main = *
    lda status_ctr ; see if we're to show status now
    bne main__nostatus
    lda #37 ; set foreground color white
    jsr txcolor
    lda #40 ; set background color black
    jsr txcolor
    lda #0 ; go to upper left corner
    sta go_row
    sta go_col
    jsr txpos
    jsr show_status ; show status now
    inc status_ctr ; and not for 256 more positions
    lda draw_row ; go back to drawing position
    sta go_row
    inc go_row
    lda draw_col
    sta go_col
    jsr txpos
main__nostatus = *
    inc status_ctr ; count up until we show it again
    ; show one more character cell
    lda draw_blk ; see if there's any color for it
    bne main__nocolor
    ; Compute & set the color for it.
    lda draw_col
    jsr absm18 ; compute abs(A-18)
    sta atmp
    lda draw_row
    jsr absm18
    clc
    adc atmp
    and #7
    clc
    adc #40
    jsr txcolor
main__nocolor = *
    ; draw the cell
    lda #' '
    jsr txchar
    ; go on to the next cell
    inc draw_col ; move right
    lda draw_col ; see if we got as far right as we're going to
    cmp #39
    bmi main ; if not, repeat
    ; go on to the next row of cells
    inc draw_row ; move down
    jsr txstring
    db 13, 10, 0
    lda #0 ; move to left edge
    sta draw_col
    lda draw_row ; see if we got as far right as we're going to
    cmp #39
    bmi main ; if not, repeat
    ; start over again
    lda #0 ; move to top
    sta draw_row
    lda #0 ; go to upper left corner
    sta go_row
    inc go_row
    sta go_col
    jsr txpos
    lda #1 ; switch between black & color
    eor draw_blk
    sta draw_blk
    lda #40 ; black is safe, it'll be changed soon enough if not wanted
    jsr txcolor
    jmp main ; repeat

    ; show_status(): Display a status report
show_status = *
    jsr txstring
    asc "r="
    db 0
    ldx #ctr_rx
    jsr txctr
    jsr txstring
    asc "  r+"
    db 0
    ldx #ctr_rxon
    jsr txctr
    jsr txstring
    asc "  r-"
    db 0
    ldx #ctr_rxoff
    jsr txctr
    jsr txstring
    asc "  t="
    db 0
    ldx #ctr_tx
    jsr txctr
    jsr txstring
    asc "  t+"
    db 0
    ldx #ctr_txon
    jsr txctr
    jsr txstring
    asc "  t-"
    db 0
    ldx #ctr_txoff
    jsr txctr
    jsr txstring
    asc "      "
    db 0
    rts

    ; txcolor(): Transmit the sequence that sets a color or some other
    ; character attribute.  Indicated by the number in A.
txcolor = *
    pha ; stash the value for later
    lda #27 ; transmit the start of the escape sequence
    jsr txchar
    lda #'['
    jsr txchar
    pla ; get back the value
    jsr txnum ; transmit it
    lda #'m' ; end the escape sequence
    jmp txchar ; transmit it and return

    ; txpos(): Transmit the sequence that moves the cursor to the
    ; position in go_row & go_col.
txpos = *
    lda #27 ; transmit the start of the escape sequence
    jsr txchar
    lda #'['
    jsr txchar
    ldx go_row ; row number ; convert it to 1 based
    inx
    txa
    jsr txnum
    lda #';' ; separator char
    jsr txchar
    ldx go_col ; column number ; convert it to 1 based
    inx
    txa
    jsr txnum
    lda #'H' ; end the escape sequence
    jmp txchar ; transmit it and return

    ; absm18 - compute a=abs(a-18)
absm18 = *
    clc ; subtract 18
    sbc #18
    bpl absm18__return ; if positive, we're done
    ; if negative: negate it
    eor #$ff ; this computes -1-a
    tax
    inx
    txa ; and that's -a
absm18__return = *
    rts

    ; txstring: transmit a zero terminated byte string via txchar().
    ; the byte string follows the jsr to trstring.
txstring__nonzero = *
    jsr txchar
txstring = *
    jsr raget ; get a byte after the jsr to txstring
    bne txstring__nonzero ; if byte nonzero: print and continue
    ; the byte was zero; we're done
    rts

    ; raget: Increment its *caller's* return address and return the byte
    ; the new pointer goes to.  That allows creation of a subroutine that
    ; takes hard coded parameters right after the 'jsr' and returns to right
    ; after them.
    ; Notes about stack:
    ;   + what's on stack is return address minus 1
    ;   + stack grows from $1FF to $100
    ;   + stack pointer points to where the next free byte is
raget = *
    tsx ; get the stack pointer
    lda $103,x ; increment the caller address & copy to atmp
    clc
    adc #1
    sta atmp
    sta $103,x
    lda $104,x
    adc #0
    sta $104,x
    sta atmp+1
    ldy #0 ; now we'll use atmp to get the byte
    lda (atmp),y
    rts ; and now we're done

    ; txctr() - Transmit the 8 digit loose decimal counter pointed to by
    ; X (found in zero page).
txctr = *
    ldy #8 ; Y will count the 8 characters to go
txctr__loop = *
    lda 0,x ; get the digit
    clc
    adc #'0' ; convert to ASCII
    jsr txchar
    inx ; next digit
    dey ; one less digit to go
    bne txctr__loop ; branch if there are more to be done
    rts

    ; txnum() - Transmit an integer value, in decimal, from the accumulator.
    ; Uses lookup tables, because it's easier than division.
txnum = *
    tax ; put the value in X register for easier lookup
    lda txnum_tbl100,x ; get 100's digit if any
    beq txnum__10
    jsr txchar ; transmit it
txnum__10 = *
    lda txnum_tbl10,x ; get 10's digit if any
    beq txnum__1
    jsr txchar ; transmit it
txnum__1 = *
    lda txnum_tbl1,x ; get 1's digit
    jmp txchar ; transmit it & return

    ; txchar() - Transmit a single byte (ASCII character) value in the
    ; accumulator.  This also gets into the whole event loop for transmitting
    ; and receiving bytes, including both data & XON/XOFF.  This function
    ; preserves registers A, X, Y.
txchar = *
    ; preserve register values
    sta preserve_a
    stx preserve_x
    sty preserve_y
    ; count on the LEDs
    inc LEDOUT
txchar__loop = *
    ; see if there are bytes to receive
    lda ICTL_PEA1
    ror a ; get bit 0 into carry
    bcc txchar__norx
    ; receive a byte
    lda SER_BASE+SER_ST ; serial status byte
    ror a ; get bit 0 into carry
    bcs txchar__rxpanic
    lda SER_BASE+SER_RX ; byte received
    sta SER_BASE+SER_RX ; remove it from the buffer
    cmp #XON_CHAR ; is it XON?
    beq txchar__rxxon
    cmp #XOFF_CHAR ; is it XOFF?
    beq txchar__rxxoff
    ; received a byte: not XON or XOFF, just count it
    ldx #ctr_rx
    jsr inc_ctr
    jmp txchar__loop ; and see what else we can do
txchar__rxxon = *
    ; received an XON byte; count it & honor it
    lda #0
    sta xoffed
    ldx #ctr_rxon
    jsr inc_ctr
    jmp txchar__loop ; and see what else we can do
txchar__rxxoff = *
    ; received an XOFF byte; count it & honor it
    lda #1
    sta xoffed
    ldx #ctr_rxoff
    jsr inc_ctr
    jmp txchar__loop ; and see what else we can do
txchar__rxpanic = *
    ; an error has come in on the serial port
    lda #'R'
    jmp panic
txchar__norx = *
    ; No byte to receive; can we transmit a byte?
    lda ICTL_PEB1
    ror a ; get bit 0 into carry, indicating port 8 is ready for write
    bcc txchar__loop ; not ready; nothing we can do now
    ; We can transmit a byte, but what will it be?  Normally it'll
    ; be the byte value stored in preserve_a, but occasionally XON or
    ; XOFF is sent first.
    jsr prng ; get a pseudo random byte into the accumulator
    tax ; stash the value, we may use it twice
    and #3 ; the low 2 bits determine whether we transmit an XON/XOFF
    beq txchar__txxonxoff
    ; We transmit the byte we intended to all along.
    lda preserve_a
    sta SER_BASE+SER_TX ; write the byte
    ; Count it
    ldx #ctr_tx
    jsr inc_ctr
    ; Having transmitted the byte, we can return, after restoring registers.
    ldx preserve_x
    ldy preserve_y
    rts
txchar__txxonxoff = *
    ; We're going to transmit XON or XOFF.  Which will it be?
    ; Pseudorandomly chosen.
    txa ; we saved the last pseudorandom number here
    and #12 ; the next 2 bits tell us which we're going to do
    beq txchar__txxon ; XON is less common than XOFF
    ; We're going to transmit XOFF.
    lda #XOFF_CHAR ; write that byte
    sta SER_BASE+SER_TX
    ldx #ctr_txoff ; count it
    jsr inc_ctr
    jmp txchar__loop ; and see what else we can do
txchar__txxon = *
    ; We're going to transmit XON.
    lda #XON_CHAR ; write that byte
    sta SER_BASE+SER_TX
    ldx #ctr_txon ; count it
    jsr inc_ctr
    jmp txchar__loop ; and see what else we can do

    ; prng() - Pseudo random number generation.  This is probably not good
    ; but I don't feel like more, and it isn't for a good cause, the only
    ; reason the PRNG is here is to avoid obvious patterns.
    ; This function updates the 4-byte state vector and then generates a
    ; single byte from the new state vector, returning it in the accumulator.
    ; The bytes of the state vector are updated as follows:
    ;       byte 0 - by a lookup table that gives period 256
    ;       byte 1 - by a lookup table that gives period 255
    ;       bytes 2-3 - adding bytes 0-1 as a 16 bit integer
    ; Byte 3 is then passed through the byte-0 lookup table to get the result.
prng = *
    ldx prngs+1 ; perform the transform on byte one
    lda prng_tbl1,x
    sta prngs+1
    ldx prngs ; perform the transform on byte zero
    lda prng_tbl0,x
    sta prngs
    clc ; and perform the 16 bit addition
    adc prngs+2
    sta prngs+2
    lda prngs+3
    adc prngs+1
    sta prngs+3
    ldx prngs+3 ; and compute the result
    lda prng_tbl0,x
    rts ; and return it

    ; inc_ctr() - Increment an 8 byte decimal counter pointed to by X.
inc_ctr = *
inc_ctr_1digit macro
    inc \1,x ; increment the digit
    lda \1,x ; and check to see if it carries
    cmp #10
    bmi inc_ctr__ret ; branch if it doesn't carry
    lda #0 ; if it carries, take it back to zero
    sta \1,x
endm
    inc_ctr_1digit 7 ; 1's digit
    inc_ctr_1digit 6 ; 10's digit
    inc_ctr_1digit 5 ; 100's digit
    inc_ctr_1digit 4 ; 1,000's digit
    inc_ctr_1digit 3 ; 10,000's digit
    inc_ctr_1digit 2 ; 100,000's digit
    inc_ctr_1digit 1 ; 1,000,000's digit
    inc_ctr_1digit 0 ; 10,000,000's digit
inc_ctr__ret = *
    rts

    ; panic(): simple function for system crash.  Sets LEDs and prints a
    ; character on the output serial port.
panic = *
    sei ; don't be interrupted
    sta LEDOUT ; put it on LEDs
    sta SER_BASE+SER_TX ; put it in output FIFO even if FIFO is full
    jmp panic

irqpanic = *
    lda #'I'
    jmp panic

    ; pgalign(): Align whatever followed to a page (256 byte) boundary
    ; in memory.
pgalign MACRO
    * = (* + 255) & 65280
ENDM

    ; lookup tables for txnum(), mapping numeric values 0-255 to digit
    ; character values, or 0 if there's nothing there.
pgalign
txnum_tbl100 = *
    db 0 ; 0
    db 0 ; 1
    db 0 ; 2
    db 0 ; 3
    db 0 ; 4
    db 0 ; 5
    db 0 ; 6
    db 0 ; 7
    db 0 ; 8
    db 0 ; 9
    db 0 ; 10
    db 0 ; 11
    db 0 ; 12
    db 0 ; 13
    db 0 ; 14
    db 0 ; 15
    db 0 ; 16
    db 0 ; 17
    db 0 ; 18
    db 0 ; 19
    db 0 ; 20
    db 0 ; 21
    db 0 ; 22
    db 0 ; 23
    db 0 ; 24
    db 0 ; 25
    db 0 ; 26
    db 0 ; 27
    db 0 ; 28
    db 0 ; 29
    db 0 ; 30
    db 0 ; 31
    db 0 ; 32
    db 0 ; 33
    db 0 ; 34
    db 0 ; 35
    db 0 ; 36
    db 0 ; 37
    db 0 ; 38
    db 0 ; 39
    db 0 ; 40
    db 0 ; 41
    db 0 ; 42
    db 0 ; 43
    db 0 ; 44
    db 0 ; 45
    db 0 ; 46
    db 0 ; 47
    db 0 ; 48
    db 0 ; 49
    db 0 ; 50
    db 0 ; 51
    db 0 ; 52
    db 0 ; 53
    db 0 ; 54
    db 0 ; 55
    db 0 ; 56
    db 0 ; 57
    db 0 ; 58
    db 0 ; 59
    db 0 ; 60
    db 0 ; 61
    db 0 ; 62
    db 0 ; 63
    db 0 ; 64
    db 0 ; 65
    db 0 ; 66
    db 0 ; 67
    db 0 ; 68
    db 0 ; 69
    db 0 ; 70
    db 0 ; 71
    db 0 ; 72
    db 0 ; 73
    db 0 ; 74
    db 0 ; 75
    db 0 ; 76
    db 0 ; 77
    db 0 ; 78
    db 0 ; 79
    db 0 ; 80
    db 0 ; 81
    db 0 ; 82
    db 0 ; 83
    db 0 ; 84
    db 0 ; 85
    db 0 ; 86
    db 0 ; 87
    db 0 ; 88
    db 0 ; 89
    db 0 ; 90
    db 0 ; 91
    db 0 ; 92
    db 0 ; 93
    db 0 ; 94
    db 0 ; 95
    db 0 ; 96
    db 0 ; 97
    db 0 ; 98
    db 0 ; 99
    db 49 ; 100
    db 49 ; 101
    db 49 ; 102
    db 49 ; 103
    db 49 ; 104
    db 49 ; 105
    db 49 ; 106
    db 49 ; 107
    db 49 ; 108
    db 49 ; 109
    db 49 ; 110
    db 49 ; 111
    db 49 ; 112
    db 49 ; 113
    db 49 ; 114
    db 49 ; 115
    db 49 ; 116
    db 49 ; 117
    db 49 ; 118
    db 49 ; 119
    db 49 ; 120
    db 49 ; 121
    db 49 ; 122
    db 49 ; 123
    db 49 ; 124
    db 49 ; 125
    db 49 ; 126
    db 49 ; 127
    db 49 ; 128
    db 49 ; 129
    db 49 ; 130
    db 49 ; 131
    db 49 ; 132
    db 49 ; 133
    db 49 ; 134
    db 49 ; 135
    db 49 ; 136
    db 49 ; 137
    db 49 ; 138
    db 49 ; 139
    db 49 ; 140
    db 49 ; 141
    db 49 ; 142
    db 49 ; 143
    db 49 ; 144
    db 49 ; 145
    db 49 ; 146
    db 49 ; 147
    db 49 ; 148
    db 49 ; 149
    db 49 ; 150
    db 49 ; 151
    db 49 ; 152
    db 49 ; 153
    db 49 ; 154
    db 49 ; 155
    db 49 ; 156
    db 49 ; 157
    db 49 ; 158
    db 49 ; 159
    db 49 ; 160
    db 49 ; 161
    db 49 ; 162
    db 49 ; 163
    db 49 ; 164
    db 49 ; 165
    db 49 ; 166
    db 49 ; 167
    db 49 ; 168
    db 49 ; 169
    db 49 ; 170
    db 49 ; 171
    db 49 ; 172
    db 49 ; 173
    db 49 ; 174
    db 49 ; 175
    db 49 ; 176
    db 49 ; 177
    db 49 ; 178
    db 49 ; 179
    db 49 ; 180
    db 49 ; 181
    db 49 ; 182
    db 49 ; 183
    db 49 ; 184
    db 49 ; 185
    db 49 ; 186
    db 49 ; 187
    db 49 ; 188
    db 49 ; 189
    db 49 ; 190
    db 49 ; 191
    db 49 ; 192
    db 49 ; 193
    db 49 ; 194
    db 49 ; 195
    db 49 ; 196
    db 49 ; 197
    db 49 ; 198
    db 49 ; 199
    db 50 ; 200
    db 50 ; 201
    db 50 ; 202
    db 50 ; 203
    db 50 ; 204
    db 50 ; 205
    db 50 ; 206
    db 50 ; 207
    db 50 ; 208
    db 50 ; 209
    db 50 ; 210
    db 50 ; 211
    db 50 ; 212
    db 50 ; 213
    db 50 ; 214
    db 50 ; 215
    db 50 ; 216
    db 50 ; 217
    db 50 ; 218
    db 50 ; 219
    db 50 ; 220
    db 50 ; 221
    db 50 ; 222
    db 50 ; 223
    db 50 ; 224
    db 50 ; 225
    db 50 ; 226
    db 50 ; 227
    db 50 ; 228
    db 50 ; 229
    db 50 ; 230
    db 50 ; 231
    db 50 ; 232
    db 50 ; 233
    db 50 ; 234
    db 50 ; 235
    db 50 ; 236
    db 50 ; 237
    db 50 ; 238
    db 50 ; 239
    db 50 ; 240
    db 50 ; 241
    db 50 ; 242
    db 50 ; 243
    db 50 ; 244
    db 50 ; 245
    db 50 ; 246
    db 50 ; 247
    db 50 ; 248
    db 50 ; 249
    db 50 ; 250
    db 50 ; 251
    db 50 ; 252
    db 50 ; 253
    db 50 ; 254
    db 50 ; 255
txnum_tbl10 = *
    db 0 ; 0 
    db 0 ; 1 
    db 0 ; 2 
    db 0 ; 3 
    db 0 ; 4 
    db 0 ; 5 
    db 0 ; 6 
    db 0 ; 7 
    db 0 ; 8 
    db 0 ; 9 
    db 49 ; 10 
    db 49 ; 11 
    db 49 ; 12 
    db 49 ; 13 
    db 49 ; 14 
    db 49 ; 15 
    db 49 ; 16 
    db 49 ; 17 
    db 49 ; 18 
    db 49 ; 19 
    db 50 ; 20 
    db 50 ; 21 
    db 50 ; 22 
    db 50 ; 23 
    db 50 ; 24 
    db 50 ; 25 
    db 50 ; 26 
    db 50 ; 27 
    db 50 ; 28 
    db 50 ; 29 
    db 51 ; 30 
    db 51 ; 31 
    db 51 ; 32 
    db 51 ; 33 
    db 51 ; 34 
    db 51 ; 35 
    db 51 ; 36 
    db 51 ; 37 
    db 51 ; 38 
    db 51 ; 39 
    db 52 ; 40 
    db 52 ; 41 
    db 52 ; 42 
    db 52 ; 43 
    db 52 ; 44 
    db 52 ; 45 
    db 52 ; 46 
    db 52 ; 47 
    db 52 ; 48 
    db 52 ; 49 
    db 53 ; 50 
    db 53 ; 51 
    db 53 ; 52 
    db 53 ; 53 
    db 53 ; 54 
    db 53 ; 55 
    db 53 ; 56 
    db 53 ; 57 
    db 53 ; 58 
    db 53 ; 59 
    db 54 ; 60 
    db 54 ; 61 
    db 54 ; 62 
    db 54 ; 63 
    db 54 ; 64 
    db 54 ; 65 
    db 54 ; 66 
    db 54 ; 67 
    db 54 ; 68 
    db 54 ; 69 
    db 55 ; 70 
    db 55 ; 71 
    db 55 ; 72 
    db 55 ; 73 
    db 55 ; 74 
    db 55 ; 75 
    db 55 ; 76 
    db 55 ; 77 
    db 55 ; 78 
    db 55 ; 79 
    db 56 ; 80 
    db 56 ; 81 
    db 56 ; 82 
    db 56 ; 83 
    db 56 ; 84 
    db 56 ; 85 
    db 56 ; 86 
    db 56 ; 87 
    db 56 ; 88 
    db 56 ; 89 
    db 57 ; 90 
    db 57 ; 91 
    db 57 ; 92 
    db 57 ; 93 
    db 57 ; 94 
    db 57 ; 95 
    db 57 ; 96 
    db 57 ; 97 
    db 57 ; 98 
    db 57 ; 99 
    db 48 ; 100 
    db 48 ; 101 
    db 48 ; 102 
    db 48 ; 103 
    db 48 ; 104 
    db 48 ; 105 
    db 48 ; 106 
    db 48 ; 107 
    db 48 ; 108 
    db 48 ; 109 
    db 49 ; 110 
    db 49 ; 111 
    db 49 ; 112 
    db 49 ; 113 
    db 49 ; 114 
    db 49 ; 115 
    db 49 ; 116 
    db 49 ; 117 
    db 49 ; 118 
    db 49 ; 119 
    db 50 ; 120 
    db 50 ; 121 
    db 50 ; 122 
    db 50 ; 123 
    db 50 ; 124 
    db 50 ; 125 
    db 50 ; 126 
    db 50 ; 127 
    db 50 ; 128 
    db 50 ; 129 
    db 51 ; 130 
    db 51 ; 131 
    db 51 ; 132 
    db 51 ; 133 
    db 51 ; 134 
    db 51 ; 135 
    db 51 ; 136 
    db 51 ; 137 
    db 51 ; 138 
    db 51 ; 139 
    db 52 ; 140 
    db 52 ; 141 
    db 52 ; 142 
    db 52 ; 143 
    db 52 ; 144 
    db 52 ; 145 
    db 52 ; 146 
    db 52 ; 147 
    db 52 ; 148 
    db 52 ; 149 
    db 53 ; 150 
    db 53 ; 151 
    db 53 ; 152 
    db 53 ; 153 
    db 53 ; 154 
    db 53 ; 155 
    db 53 ; 156 
    db 53 ; 157 
    db 53 ; 158 
    db 53 ; 159 
    db 54 ; 160 
    db 54 ; 161 
    db 54 ; 162 
    db 54 ; 163 
    db 54 ; 164 
    db 54 ; 165 
    db 54 ; 166 
    db 54 ; 167 
    db 54 ; 168 
    db 54 ; 169 
    db 55 ; 170 
    db 55 ; 171 
    db 55 ; 172 
    db 55 ; 173 
    db 55 ; 174 
    db 55 ; 175 
    db 55 ; 176 
    db 55 ; 177 
    db 55 ; 178 
    db 55 ; 179 
    db 56 ; 180 
    db 56 ; 181 
    db 56 ; 182 
    db 56 ; 183 
    db 56 ; 184 
    db 56 ; 185 
    db 56 ; 186 
    db 56 ; 187 
    db 56 ; 188 
    db 56 ; 189 
    db 57 ; 190 
    db 57 ; 191 
    db 57 ; 192 
    db 57 ; 193 
    db 57 ; 194 
    db 57 ; 195 
    db 57 ; 196 
    db 57 ; 197 
    db 57 ; 198 
    db 57 ; 199 
    db 48 ; 200 
    db 48 ; 201 
    db 48 ; 202 
    db 48 ; 203 
    db 48 ; 204 
    db 48 ; 205 
    db 48 ; 206 
    db 48 ; 207 
    db 48 ; 208 
    db 48 ; 209 
    db 49 ; 210 
    db 49 ; 211 
    db 49 ; 212 
    db 49 ; 213 
    db 49 ; 214 
    db 49 ; 215 
    db 49 ; 216 
    db 49 ; 217 
    db 49 ; 218 
    db 49 ; 219 
    db 50 ; 220 
    db 50 ; 221 
    db 50 ; 222 
    db 50 ; 223 
    db 50 ; 224 
    db 50 ; 225 
    db 50 ; 226 
    db 50 ; 227 
    db 50 ; 228 
    db 50 ; 229 
    db 51 ; 230 
    db 51 ; 231 
    db 51 ; 232 
    db 51 ; 233 
    db 51 ; 234 
    db 51 ; 235 
    db 51 ; 236 
    db 51 ; 237 
    db 51 ; 238 
    db 51 ; 239 
    db 52 ; 240 
    db 52 ; 241 
    db 52 ; 242 
    db 52 ; 243 
    db 52 ; 244 
    db 52 ; 245 
    db 52 ; 246 
    db 52 ; 247 
    db 52 ; 248 
    db 52 ; 249 
    db 53 ; 250 
    db 53 ; 251 
    db 53 ; 252 
    db 53 ; 253 
    db 53 ; 254 
    db 53 ; 255 
txnum_tbl1 = *
    db 48 ; 0 
    db 49 ; 1 
    db 50 ; 2 
    db 51 ; 3 
    db 52 ; 4 
    db 53 ; 5 
    db 54 ; 6 
    db 55 ; 7 
    db 56 ; 8 
    db 57 ; 9 
    db 48 ; 10 
    db 49 ; 11 
    db 50 ; 12 
    db 51 ; 13 
    db 52 ; 14 
    db 53 ; 15 
    db 54 ; 16 
    db 55 ; 17 
    db 56 ; 18 
    db 57 ; 19 
    db 48 ; 20 
    db 49 ; 21 
    db 50 ; 22 
    db 51 ; 23 
    db 52 ; 24 
    db 53 ; 25 
    db 54 ; 26 
    db 55 ; 27 
    db 56 ; 28 
    db 57 ; 29 
    db 48 ; 30 
    db 49 ; 31 
    db 50 ; 32 
    db 51 ; 33 
    db 52 ; 34 
    db 53 ; 35 
    db 54 ; 36 
    db 55 ; 37 
    db 56 ; 38 
    db 57 ; 39 
    db 48 ; 40 
    db 49 ; 41 
    db 50 ; 42 
    db 51 ; 43 
    db 52 ; 44 
    db 53 ; 45 
    db 54 ; 46 
    db 55 ; 47 
    db 56 ; 48 
    db 57 ; 49 
    db 48 ; 50 
    db 49 ; 51 
    db 50 ; 52 
    db 51 ; 53 
    db 52 ; 54 
    db 53 ; 55 
    db 54 ; 56 
    db 55 ; 57 
    db 56 ; 58 
    db 57 ; 59 
    db 48 ; 60 
    db 49 ; 61 
    db 50 ; 62 
    db 51 ; 63 
    db 52 ; 64 
    db 53 ; 65 
    db 54 ; 66 
    db 55 ; 67 
    db 56 ; 68 
    db 57 ; 69 
    db 48 ; 70 
    db 49 ; 71 
    db 50 ; 72 
    db 51 ; 73 
    db 52 ; 74 
    db 53 ; 75 
    db 54 ; 76 
    db 55 ; 77 
    db 56 ; 78 
    db 57 ; 79 
    db 48 ; 80 
    db 49 ; 81 
    db 50 ; 82 
    db 51 ; 83 
    db 52 ; 84 
    db 53 ; 85 
    db 54 ; 86 
    db 55 ; 87 
    db 56 ; 88 
    db 57 ; 89 
    db 48 ; 90 
    db 49 ; 91 
    db 50 ; 92 
    db 51 ; 93 
    db 52 ; 94 
    db 53 ; 95 
    db 54 ; 96 
    db 55 ; 97 
    db 56 ; 98 
    db 57 ; 99 
    db 48 ; 100 
    db 49 ; 101 
    db 50 ; 102 
    db 51 ; 103 
    db 52 ; 104 
    db 53 ; 105 
    db 54 ; 106 
    db 55 ; 107 
    db 56 ; 108 
    db 57 ; 109 
    db 48 ; 110 
    db 49 ; 111 
    db 50 ; 112 
    db 51 ; 113 
    db 52 ; 114 
    db 53 ; 115 
    db 54 ; 116 
    db 55 ; 117 
    db 56 ; 118 
    db 57 ; 119 
    db 48 ; 120 
    db 49 ; 121 
    db 50 ; 122 
    db 51 ; 123 
    db 52 ; 124 
    db 53 ; 125 
    db 54 ; 126 
    db 55 ; 127 
    db 56 ; 128 
    db 57 ; 129 
    db 48 ; 130 
    db 49 ; 131 
    db 50 ; 132 
    db 51 ; 133 
    db 52 ; 134 
    db 53 ; 135 
    db 54 ; 136 
    db 55 ; 137 
    db 56 ; 138 
    db 57 ; 139 
    db 48 ; 140 
    db 49 ; 141 
    db 50 ; 142 
    db 51 ; 143 
    db 52 ; 144 
    db 53 ; 145 
    db 54 ; 146 
    db 55 ; 147 
    db 56 ; 148 
    db 57 ; 149 
    db 48 ; 150 
    db 49 ; 151 
    db 50 ; 152 
    db 51 ; 153 
    db 52 ; 154 
    db 53 ; 155 
    db 54 ; 156 
    db 55 ; 157 
    db 56 ; 158 
    db 57 ; 159 
    db 48 ; 160 
    db 49 ; 161 
    db 50 ; 162 
    db 51 ; 163 
    db 52 ; 164 
    db 53 ; 165 
    db 54 ; 166 
    db 55 ; 167 
    db 56 ; 168 
    db 57 ; 169 
    db 48 ; 170 
    db 49 ; 171 
    db 50 ; 172 
    db 51 ; 173 
    db 52 ; 174 
    db 53 ; 175 
    db 54 ; 176 
    db 55 ; 177 
    db 56 ; 178 
    db 57 ; 179 
    db 48 ; 180 
    db 49 ; 181 
    db 50 ; 182 
    db 51 ; 183 
    db 52 ; 184 
    db 53 ; 185 
    db 54 ; 186 
    db 55 ; 187 
    db 56 ; 188 
    db 57 ; 189 
    db 48 ; 190 
    db 49 ; 191 
    db 50 ; 192 
    db 51 ; 193 
    db 52 ; 194 
    db 53 ; 195 
    db 54 ; 196 
    db 55 ; 197 
    db 56 ; 198 
    db 57 ; 199 
    db 48 ; 200 
    db 49 ; 201 
    db 50 ; 202 
    db 51 ; 203 
    db 52 ; 204 
    db 53 ; 205 
    db 54 ; 206 
    db 55 ; 207 
    db 56 ; 208 
    db 57 ; 209 
    db 48 ; 210 
    db 49 ; 211 
    db 50 ; 212 
    db 51 ; 213 
    db 52 ; 214 
    db 53 ; 215 
    db 54 ; 216 
    db 55 ; 217 
    db 56 ; 218 
    db 57 ; 219 
    db 48 ; 220 
    db 49 ; 221 
    db 50 ; 222 
    db 51 ; 223 
    db 52 ; 224 
    db 53 ; 225 
    db 54 ; 226 
    db 55 ; 227 
    db 56 ; 228 
    db 57 ; 229 
    db 48 ; 230 
    db 49 ; 231 
    db 50 ; 232 
    db 51 ; 233 
    db 52 ; 234 
    db 53 ; 235 
    db 54 ; 236 
    db 55 ; 237 
    db 56 ; 238 
    db 57 ; 239 
    db 48 ; 240 
    db 49 ; 241 
    db 50 ; 242 
    db 51 ; 243 
    db 52 ; 244 
    db 53 ; 245 
    db 54 ; 246 
    db 55 ; 247 
    db 56 ; 248 
    db 57 ; 249 
    db 48 ; 250 
    db 49 ; 251 
    db 50 ; 252 
    db 51 ; 253 
    db 52 ; 254 
    db 53 ; 255 

    ; lookup tables for prng(): prng_tbl0 gives a period of 256, while
    ; prng_tbl1 gives a period of 255.
prng_tbl0 = *
    db 205 ; 0 
    db 11 ; 1 
    db 37 ; 2 
    db 187 ; 3 
    db 56 ; 4 
    db 227 ; 5 
    db 170 ; 6 
    db 210 ; 7 
    db 174 ; 8 
    db 55 ; 9 
    db 175 ; 10 
    db 151 ; 11 
    db 147 ; 12 
    db 192 ; 13 
    db 231 ; 14 
    db 228 ; 15 
    db 1 ; 16 
    db 106 ; 17 
    db 140 ; 18 
    db 25 ; 19 
    db 16 ; 20 
    db 153 ; 21 
    db 173 ; 22 
    db 14 ; 23 
    db 42 ; 24 
    db 203 ; 25 
    db 122 ; 26 
    db 252 ; 27 
    db 176 ; 28 
    db 46 ; 29 
    db 34 ; 30 
    db 49 ; 31 
    db 77 ; 32 
    db 41 ; 33 
    db 224 ; 34 
    db 230 ; 35 
    db 155 ; 36 
    db 190 ; 37 
    db 139 ; 38 
    db 238 ; 39 
    db 0 ; 40 
    db 250 ; 41 
    db 78 ; 42 
    db 183 ; 43 
    db 2 ; 44 
    db 20 ; 45 
    db 245 ; 46 
    db 66 ; 47 
    db 138 ; 48 
    db 247 ; 49 
    db 19 ; 50 
    db 74 ; 51 
    db 61 ; 52 
    db 134 ; 53 
    db 219 ; 54 
    db 164 ; 55 
    db 98 ; 56 
    db 117 ; 57 
    db 225 ; 58 
    db 209 ; 59 
    db 165 ; 60 
    db 72 ; 61 
    db 246 ; 62 
    db 87 ; 63 
    db 167 ; 64 
    db 107 ; 65 
    db 242 ; 66 
    db 23 ; 67 
    db 93 ; 68 
    db 71 ; 69 
    db 251 ; 70 
    db 114 ; 71 
    db 234 ; 72 
    db 9 ; 73 
    db 157 ; 74 
    db 51 ; 75 
    db 255 ; 76 
    db 101 ; 77 
    db 99 ; 78 
    db 10 ; 79 
    db 119 ; 80 
    db 199 ; 81 
    db 65 ; 82 
    db 133 ; 83 
    db 83 ; 84 
    db 110 ; 85 
    db 211 ; 86 
    db 58 ; 87 
    db 73 ; 88 
    db 94 ; 89 
    db 27 ; 90 
    db 208 ; 91 
    db 109 ; 92 
    db 123 ; 93 
    db 235 ; 94 
    db 244 ; 95 
    db 44 ; 96 
    db 243 ; 97 
    db 111 ; 98 
    db 131 ; 99 
    db 85 ; 100 
    db 43 ; 101 
    db 45 ; 102 
    db 70 ; 103 
    db 239 ; 104 
    db 169 ; 105 
    db 76 ; 106 
    db 159 ; 107 
    db 236 ; 108 
    db 8 ; 109 
    db 201 ; 110 
    db 177 ; 111 
    db 156 ; 112 
    db 67 ; 113 
    db 194 ; 114 
    db 160 ; 115 
    db 40 ; 116 
    db 222 ; 117 
    db 163 ; 118 
    db 132 ; 119 
    db 216 ; 120 
    db 179 ; 121 
    db 59 ; 122 
    db 137 ; 123 
    db 90 ; 124 
    db 143 ; 125 
    db 136 ; 126 
    db 215 ; 127 
    db 221 ; 128 
    db 80 ; 129 
    db 92 ; 130 
    db 212 ; 131 
    db 178 ; 132 
    db 142 ; 133 
    db 204 ; 134 
    db 113 ; 135 
    db 91 ; 136 
    db 18 ; 137 
    db 28 ; 138 
    db 3 ; 139 
    db 7 ; 140 
    db 53 ; 141 
    db 79 ; 142 
    db 181 ; 143 
    db 198 ; 144 
    db 68 ; 145 
    db 35 ; 146 
    db 60 ; 147 
    db 52 ; 148 
    db 195 ; 149 
    db 237 ; 150 
    db 108 ; 151 
    db 191 ; 152 
    db 100 ; 153 
    db 168 ; 154 
    db 188 ; 155 
    db 248 ; 156 
    db 149 ; 157 
    db 38 ; 158 
    db 118 ; 159 
    db 88 ; 160 
    db 184 ; 161 
    db 75 ; 162 
    db 102 ; 163 
    db 62 ; 164 
    db 29 ; 165 
    db 31 ; 166 
    db 213 ; 167 
    db 232 ; 168 
    db 103 ; 169 
    db 124 ; 170 
    db 36 ; 171 
    db 95 ; 172 
    db 6 ; 173 
    db 5 ; 174 
    db 254 ; 175 
    db 84 ; 176 
    db 145 ; 177 
    db 128 ; 178 
    db 48 ; 179 
    db 30 ; 180 
    db 21 ; 181 
    db 129 ; 182 
    db 197 ; 183 
    db 112 ; 184 
    db 81 ; 185 
    db 182 ; 186 
    db 152 ; 187 
    db 22 ; 188 
    db 125 ; 189 
    db 162 ; 190 
    db 54 ; 191 
    db 86 ; 192 
    db 161 ; 193 
    db 13 ; 194 
    db 24 ; 195 
    db 82 ; 196 
    db 146 ; 197 
    db 206 ; 198 
    db 226 ; 199 
    db 233 ; 200 
    db 127 ; 201 
    db 171 ; 202 
    db 115 ; 203 
    db 26 ; 204 
    db 64 ; 205 
    db 196 ; 206 
    db 223 ; 207 
    db 220 ; 208 
    db 144 ; 209 
    db 69 ; 210 
    db 214 ; 211 
    db 126 ; 212 
    db 218 ; 213 
    db 186 ; 214 
    db 63 ; 215 
    db 47 ; 216 
    db 240 ; 217 
    db 253 ; 218 
    db 105 ; 219 
    db 150 ; 220 
    db 141 ; 221 
    db 32 ; 222 
    db 39 ; 223 
    db 4 ; 224 
    db 104 ; 225 
    db 97 ; 226 
    db 116 ; 227 
    db 148 ; 228 
    db 185 ; 229 
    db 207 ; 230 
    db 89 ; 231 
    db 33 ; 232 
    db 154 ; 233 
    db 121 ; 234 
    db 12 ; 235 
    db 200 ; 236 
    db 202 ; 237 
    db 172 ; 238 
    db 15 ; 239 
    db 241 ; 240 
    db 17 ; 241 
    db 189 ; 242 
    db 193 ; 243 
    db 180 ; 244 
    db 158 ; 245 
    db 135 ; 246 
    db 50 ; 247 
    db 120 ; 248 
    db 96 ; 249 
    db 249 ; 250 
    db 217 ; 251 
    db 229 ; 252 
    db 57 ; 253 
    db 166 ; 254 
    db 130 ; 255 

prng_tbl1 = *
    db 37 ; 0 
    db 49 ; 1 
    db 39 ; 2 
    db 56 ; 3 
    db 18 ; 4 
    db 62 ; 5 
    db 2 ; 6 
    db 245 ; 7 
    db 43 ; 8 
    db 188 ; 9 
    db 107 ; 10 
    db 109 ; 11 
    db 66 ; 12 
    db 242 ; 13 
    db 52 ; 14 
    db 153 ; 15 
    db 181 ; 16 
    db 156 ; 17 
    db 19 ; 18 
    db 24 ; 19 
    db 187 ; 20 
    db 218 ; 21 
    db 167 ; 22 
    db 136 ; 23 
    db 190 ; 24 
    db 22 ; 25 
    db 34 ; 26 
    db 10 ; 27 
    db 209 ; 28 
    db 174 ; 29 
    db 80 ; 30 
    db 163 ; 31 
    db 185 ; 32 
    db 86 ; 33 
    db 146 ; 34 
    db 105 ; 35 
    db 1 ; 36 
    db 17 ; 37 
    db 28 ; 38 
    db 36 ; 39 
    db 125 ; 40 
    db 87 ; 41 
    db 169 ; 42 
    db 132 ; 43 
    db 82 ; 44 
    db 55 ; 45 
    db 155 ; 46 
    db 69 ; 47 
    db 198 ; 48 
    db 67 ; 49 
    db 199 ; 50 
    db 160 ; 51 
    db 74 ; 52 
    db 122 ; 53 
    db 150 ; 54 
    db 63 ; 55 
    db 133 ; 56 
    db 157 ; 57 
    db 131 ; 58 
    db 128 ; 59 
    db 225 ; 60 
    db 231 ; 61 
    db 103 ; 62 
    db 102 ; 63 
    db 27 ; 64 
    db 210 ; 65 
    db 207 ; 66 
    db 246 ; 67 
    db 76 ; 68 
    db 202 ; 69 
    db 192 ; 70 
    db 116 ; 71 
    db 8 ; 72 
    db 98 ; 73 
    db 40 ; 74 
    db 175 ; 75 
    db 29 ; 76 
    db 92 ; 77 
    db 186 ; 78 
    db 110 ; 79 
    db 216 ; 80 
    db 41 ; 81 
    db 61 ; 82 
    db 226 ; 83 
    db 148 ; 84 
    db 46 ; 85 
    db 16 ; 86 
    db 119 ; 87 
    db 221 ; 88 
    db 20 ; 89 
    db 53 ; 90 
    db 126 ; 91 
    db 138 ; 92 
    db 117 ; 93 
    db 64 ; 94 
    db 60 ; 95 
    db 21 ; 96 
    db 113 ; 97 
    db 201 ; 98 
    db 237 ; 99 
    db 106 ; 100 
    db 164 ; 101 
    db 121 ; 102 
    db 236 ; 103 
    db 162 ; 104 
    db 112 ; 105 
    db 44 ; 106 
    db 79 ; 107 
    db 145 ; 108 
    db 248 ; 109 
    db 158 ; 110 
    db 172 ; 111 
    db 239 ; 112 
    db 50 ; 113 
    db 115 ; 114 
    db 144 ; 115 
    db 217 ; 116 
    db 72 ; 117 
    db 111 ; 118 
    db 84 ; 119 
    db 93 ; 120 
    db 15 ; 121 
    db 26 ; 122 
    db 220 ; 123 
    db 31 ; 124 
    db 213 ; 125 
    db 189 ; 126 
    db 243 ; 127 
    db 127 ; 128 
    db 254 ; 129 
    db 90 ; 130 
    db 129 ; 131 
    db 240 ; 132 
    db 194 ; 133 
    db 5 ; 134 
    db 176 ; 135 
    db 124 ; 136 
    db 203 ; 137 
    db 180 ; 138 
    db 184 ; 139 
    db 57 ; 140 
    db 173 ; 141 
    db 78 ; 142 
    db 54 ; 143 
    db 208 ; 144 
    db 200 ; 145 
    db 47 ; 146 
    db 71 ; 147 
    db 168 ; 148 
    db 191 ; 149 
    db 251 ; 150 
    db 230 ; 151 
    db 166 ; 152 
    db 137 ; 153 
    db 73 ; 154 
    db 100 ; 155 
    db 206 ; 156 
    db 204 ; 157 
    db 250 ; 158 
    db 65 ; 159 
    db 147 ; 160 
    db 99 ; 161 
    db 83 ; 162 
    db 25 ; 163 
    db 182 ; 164 
    db 212 ; 165 
    db 139 ; 166 
    db 214 ; 167 
    db 224 ; 168 
    db 88 ; 169 
    db 171 ; 170 
    db 75 ; 171 
    db 140 ; 172 
    db 151 ; 173 
    db 193 ; 174 
    db 161 ; 175 
    db 178 ; 176 
    db 135 ; 177 
    db 247 ; 178 
    db 118 ; 179 
    db 94 ; 180 
    db 195 ; 181 
    db 235 ; 182 
    db 159 ; 183 
    db 59 ; 184 
    db 81 ; 185 
    db 252 ; 186 
    db 219 ; 187 
    db 229 ; 188 
    db 32 ; 189 
    db 183 ; 190 
    db 4 ; 191 
    db 177 ; 192 
    db 211 ; 193 
    db 91 ; 194 
    db 89 ; 195 
    db 120 ; 196 
    db 228 ; 197 
    db 35 ; 198 
    db 238 ; 199 
    db 143 ; 200 
    db 12 ; 201 
    db 13 ; 202 
    db 233 ; 203 
    db 241 ; 204 
    db 114 ; 205 
    db 149 ; 206 
    db 85 ; 207 
    db 48 ; 208 
    db 7 ; 209 
    db 23 ; 210 
    db 45 ; 211 
    db 215 ; 212 
    db 244 ; 213 
    db 77 ; 214 
    db 227 ; 215 
    db 253 ; 216 
    db 130 ; 217 
    db 0 ; 218 
    db 205 ; 219 
    db 3 ; 220 
    db 232 ; 221 
    db 14 ; 222 
    db 51 ; 223 
    db 223 ; 224 
    db 11 ; 225 
    db 108 ; 226 
    db 9 ; 227 
    db 154 ; 228 
    db 170 ; 229 
    db 58 ; 230 
    db 179 ; 231 
    db 134 ; 232 
    db 33 ; 233 
    db 95 ; 234 
    db 165 ; 235 
    db 142 ; 236 
    db 68 ; 237 
    db 234 ; 238 
    db 196 ; 239 
    db 141 ; 240 
    db 152 ; 241 
    db 97 ; 242 
    db 6 ; 243 
    db 123 ; 244 
    db 30 ; 245 
    db 249 ; 246 
    db 42 ; 247 
    db 38 ; 248 
    db 96 ; 249 
    db 70 ; 250 
    db 197 ; 251 
    db 101 ; 252 
    db 104 ; 253 
    db 222 ; 254 
    db 66 ; 255 

* = $fffc
    dw start ; reset vector
    dw irqpanic ; interrupt vector
