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

; For testing my 6502 based "VTJ-1" platform, in particular the RAM.
; Takes commands to write/read 2kB chunks of memory.

; It's also "strung out" through ROM to provide a test of that.

; Intended assembler: "crasm"

    cpu 6502
    code

; Basic system info
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
ICTL_PEA0 = $b040 ; interrupt control: pending slot 0-7 alpha IRQs
ICTL_PEA1 = $b041 ; interrupt control: pending slot 8-15 alpha IRQs
ICTL_PEB0 = $b048 ; interrupt control: pending slot 0-7 beta IRQs
ICTL_PEB1 = $b049 ; interrupt control: pending slot 8-15 beta IRQs
ICTL_PEPR = $b080 ; interrupt control: highest priority pending IRQ
BAUDCODE = $15 ; code for 9600 baud
TXMODE = $30 ; code for 8n2, compatible with 8n1
RXMODE = $20 ; code for 8n1

; Zero page addresses
cb = $00 ; cyclic byte value
cm = $01 ; cyclic byte modulus value
num = $02 ; 1 byte number
pg = $03 ; page address (2 bytes)
pgc = $05 ; number of pages to go
; $06 is unused
ecks = $07 ; if we're running 'x' (1) or 'r' (0)

; And now the program.

* = $c0dd

    ; txchar: transmit a character on the serial port in slot 8, without using
    ; interrupts.  The character is in the accumulator; doesn't alter
    ; any registers.
txchar = *
    pha ; stash the byte value for later
txchar__loop = *
    lda ICTL_PEB1
    ror a ; get bit 0 into carry, indicating port 8 is ready for write
    bcc txchar__loop ; not ready
    pla
    sta SER_BASE+SER_TX ; write the byte to port 8
    rts ; done

    ; rxchar()
    ; Routine to receive a byte on the serial port in slot 8, without using
    ; interrupts.
    ;
    ; Returns with either:
    ;       . the received byte value, in the accumulator; carry flag is clear
    ;       . some exception value, in the accumulator; carry flag is set
    ;
    ; Doesn't mangle any other registers.
rxchar = *
    ; check for availability of a character or exception in the serial
    ; device's buffer, which is indicated by its alpha IRQ
    lda ICTL_PEA1
    ror a ; get bit 0 into carry
    bcc rxchar ; not ready

    ; it's ready; receive a byte or exception
    lda SER_BASE+SER_ST ; serial status byte
    ror a ; get bit 0 into carry
    lda SER_BASE+SER_RX ; byte or exception received
    sta SER_BASE+SER_RX ; remove it from the buffer to make room for another

    rts ; done

* = $c258
start = *
    ; no interrupts, we don't use them
    sei

    ; initialize serial port on slot 8
    lda #BAUDCODE
    sta SER_BASE+SER_TA
    sta SER_BASE+SER_RA
    lda #TXMODE
    sta SER_BASE+SER_TB
    lda #RXMODE
    sta SER_BASE+SER_RB

    ; blank the LEDs
    lda #0
    sta LEDOUT

    ; initialize zero page variables
    lda #1 ; cyclic byte (to read/write in memory)
    sta cb
    lda #101 ; cyclic modulus (when to reset cyclic byte)
    sta cm
    lda #0 ; numeric input
    sta num

    ; now the main loop: read command input & perform it
main_loop = *
    jsr rxchar
    cmp #'s'
    beq cmd_s_jmp
    cmp #'m'
    beq cmd_m_jmp
    cmp #'w'
    beq cmd_w_jmp
    cmp #'r'
    beq cmd_r_jmp
    cmp #'l'
    beq cmd_l_jmp
    cmp #'@'
    beq cmd_at_jmp
    sec
    sbc #'0' ; convert to digit value, assuming it is a digit
    bmi main_loop ; it's not a digit
    cmp #10 ; is it really a digit
    bpl main_loop ; nope
    ; it's a digit; so do: num = num * 10 + digit
    pha ; stash the digit
    asl num ; num * 2
    lda num
    asl a
    asl a ; num * 8
    clc
    adc num ; num * 10
    sta num
    pla ; get the digit back
    pha ; stash it again
    clc ; and add it to the number
    adc num
    sta num
    pla ; get the digit back
    clc
    adc #'0' ; convert back to a digit
    jsr txchar ; and echo it
    jmp main_loop ; and do the next digit or command

cmd_s_jmp       jmp cmd_s
cmd_m_jmp       jmp cmd_m
cmd_w_jmp       jmp cmd_w
cmd_r_jmp       jmp cmd_r
cmd_l_jmp       jmp cmd_l
cmd_at_jmp      jmp cmd_at

* = $c581
cmd_s = *
    ; the 's' command sets the cyclic byte value
    jsr txchar ; echo the command
    lda num
    sta cb
    lda #0
    sta num
    jmp main_loop

cmd_m = *
    ; the 'm' command sets the cyclic modulus value
    jsr txchar ; echo the command
    lda num
    sta cm
    lda #0
    sta num
    jmp main_loop

* = $d000
cmd_w = *
    ; the 'w' command writes to memory
    jsr txchar ; echo the command
    lda num ; 2kB block number
    asl a
    asl a
    asl a ; 256B page number
    sta pg+1 ; store page address
    lda #0
    sta pg ; page address divisible by $00
    sta num ; reset input number
    lda #8 ; 256B pages in 2kB
    sta pgc
cmd_w__oloop = *
    ; write to a page, at (pg); there are pgc left
    lda pg+1
    and #$86
    beq cmd_w__next ; skip pages 0 & 1 - we're using them
    ldy #0
cmd_w__iloop = *
    ; write to a byte, at (pg),y
    lda cb ; the cyclic byte: the one we write
    sta (pg),y
    inc cb ; and go on to the next one
    lda cb ; see if the cyclic byte resets, by checking the cyclic modulus
    cmp cm
    bne cmd_w__noreset
    lda #0 ; reset the cyclic byte
    sta cb
cmd_w__noreset = *
    iny ; index to next byte
    bne cmd_w__iloop ; branch if there are any
cmd_w__next = *
    ; this page is done; print a status character before going on to the
    ; next one
    lda #'.' ; success
    jsr txchar
    inc pg+1 ; next page
    dec pgc ; one fewer to go
    bne cmd_w__oloop ; do that page

    ; all pages have been done; ready to accept a new command
    lda #'>'
    jsr txchar
    jmp main_loop

* = $eeee
cmd_at = *
    ; the '@' command just resets the number and does nothing
    jsr txchar ; echo the command
    lda #0
    sta num
    jmp main_loop

cmd_r = *
    ; the 'r' command reads from memory and checks its contents
    jsr txchar ; echo the command
    lda num ; 2kB block number
    asl a
    asl a
    asl a ; 256B page number
    sta pg+1 ; store page address
    lda #0
    sta pg ; page address divisible by $00
    sta num ; reset the input number
    lda #8 ; 256B pages in 2kB
    sta pgc
cmd_r__oloop = *
    ; read a page, at (pg); there are pgc left
    lda pg+1
    and #$86
    beq cmd_r__next ; skip pages 0 & 1 - they weren't written
    ldy #0
cmd_r__iloop = *
    ; read a byte, at (pg),y
    lda (pg),y ; the byte in memory
    cmp cb ; and compare to cyclic byte, what we expect to read
    bne cmd_r__bad ; branches if it failed
cmd_r__ok = *
    ; we've checked the byte and printed any error; now to go on to
    ; the next one
    inc cb ; and go on to the next one
    lda cb ; see if the cyclic byte resets, by checking the cyclic modulus
    cmp cm
    bne cmd_r__noreset
    lda #0 ; reset the cyclic byte
    sta cb
cmd_r__noreset = *
    iny ; index to next byte
    bne cmd_r__iloop ; branch if there are any
    ; this page is done; print a status character before going on to the
    ; next one
cmd_r__next = *
    lda #'.' ; success
    jsr txchar
    inc pg+1 ; next page
    dec pgc ; one fewer to go
    bne cmd_r__oloop ; do that page

cmd_r__done = *
    ; all pages have been done; ready to accept a new command
    lda #'>'
    jsr txchar
    jmp main_loop

cmd_r__bad = *
    ; Some byte had the wrong value.  Print out, in hex:
    ;       address
    ;       expected value
    ;       actual value
    ;       exclamation point
    ; Then terminate.
    pha ; stash the actual value
    lda pg+1 ; upper half of address
    jsr txhex
    tya ; lower half of address
    jsr txhex
    lda #' '
    jsr txchar
    lda cb ; expected value
    jsr txhex
    lda #' '
    jsr txchar
    pla ; actual value
    jsr txhex
    lda #'!' ; error indicator
    jsr txchar
    jmp cmd_r__done

cmd_l = *
    ; 'l' command sets LEDs
    jsr txchar ; echo the command
    lda num ; set the LEDs
    sta LEDOUT
    lda #0 ; blank the number
    sta num
    jmp main_loop ; next command

hextbl = *
    ; table of hex digits
    asc "0123456789abcdef"

txhex = *
    ; txhex(): Write to the serial port two hex digits, representing
    ; the single byte that's in the accumulator.  Clobbers it and the
    ; X register.

    pha ; save the byte value for later
    ror a ; now extracting the upper half byte
    ror a
    ror a
    ror a
    and #15
    tax ; and converting to hex
    lda hextbl, x
    jsr txchar ; and transmitting it
    pla ; and get the original byte value back
    and #15 ; and extract the lower half byte
    tax ; now converting to hex
    lda hextbl, x
    jsr txchar ; and transmitting it
    rts ; and done

* = $fffc
    dw start ; reset vector

