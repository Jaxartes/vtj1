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

; For testing my 6502 based "VTJ-1" platform, in particular the "GPIO"
; (buttons and LEDs).  Doesn't test the "ROM" read/write because I've used
; that a lot in my other tests.  Doesn't particularly need to test the LEDOUT
; part, for the same reason.

; Intended assembler: "crasm"

    cpu 6502
    code

; Basic system info
LEDCNT = $b200 ; number of LEDs
BTNCNT = $b201 ; number of buttons
LEDOUT = $b280 ; write LED state
BTNIN = $b281 ; read button state

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

BAUDCODE = $15 ; code for 9600 baud
TXMODE = $30 ; code for 8n2, compatible with 8n1
RXMODE = $20 ; code for 8n1

; Zero page addresses
parm0 = $00 ; 2 byte - parameter address
oldbtns = $02 ; old button state
tmp = $03 ; for temporary use

; And now the program.

* = $c000

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

    ; initialize zero page variables
    lda #0
    sta oldbtns ; assume all buttons were up

    ; blank the LEDs
    lda #0
    sta LEDOUT

reprint = *
    ; get number of LEDs & print a message
    lda #ledmsg&255 ; message
    sta parm0
    lda #ledmsg>>8
    sta parm0+1
    jsr txstr ; print that message
    lda LEDCNT ; number of LEDs
    jsr txhex ; display in hex
    jsr txnewline ; and end the line

    ; get number of buttons & print a message
    lda #btnmsg&255 ; message
    sta parm0
    lda #btnmsg>>8
    sta parm0+1
    jsr txstr ; print the message
    lda BTNCNT ; number of buttons
    jsr txhex ; display in hex
    jsr txnewline ; and end the line

main_loop = *
    ; now the main loop: see if there's anything to do
    lda ICTL_PEA1
    ror a ; get bit 0 into carry, indicating port 8 is ready for read
    bcc do_buttons  ; nope, nothing came in
    ; a byte came in on the serial port
    sta SER_BASE+SER_RX ; remove it from the buffer
    jmp reprint ; and re-print the messages

do_buttons = *
    lda BTNIN
    and #15 ; we only expect to have four buttons
    cmp oldbtns ; compare with old state
    beq main_loop ; if nothing changed: repeat

    ; Some button changed state.  But which one, and was it up or down?
    eor oldbtns ; now A will have a '1' for the buttons that changed
    and #15 ; just to be safe, mask the bits
    tax ; we can look up which button that means
    lda btnbits,x ; pick one of them (there'll almost always be only one)
    and oldbtns ; and see if it was previously up (0) or down (1)
    beq wasup ; branch if it had been up & is thus going down
    ; it was down & is thus going up
    lda #32 ; makes lower case to indicate up
    jmp showbtn
wasup = *
    ; it was up & is thus going down
    lda #0 ; makes upper case to indicate down
showbtn = *
    ; accumulator has 32 or 0 to determine the case of the letter we
    ; print out; find out which letter by looking up in btnnames
    clc
    adc btnnames,x
    jsr txchar ; and print that character
    ; ok, now flip this button's bit in 'oldbtns' and repeat
    lda btnbits,x ; the same button bit again
    eor oldbtns ; flip that bit
    sta oldbtns ; and store it
    jmp main_loop ; and done

hextbl = *
    ; table of hex digits
    asc "0123456789abcdef"

ledmsg = *
    asc "Number of LEDs reported:    "
    db 0

btnmsg = *
    asc "Number of buttons reported: "
    db 0

    ; btnnames & btnbits are a pair of look up tables for identifying
    ; buttons.  The index into each is a 4-bit integer, the '1' bits of
    ; which each correspond to a button.  btnnames names the button,
    ; and btnbits gives its corresponding value
btnnames = *
    asc "WWSSNNNNEEEEEEEE"
btnbits = *
    db 1, 1, 2, 2, 4, 4, 4, 4, 8, 8, 8, 8, 8, 8, 8, 8

txnewline = *
    ; txnewline(): transmit carriage return & line feed
    lda #13
    jsr txchar
    lda #10
    jsr txchar
    rts

txstr = *
    ; txstr(): Transmit the null-terminated string pointed to by 'parm0'
    ldy #0 ; pointer to individual bytes of the string
txstr__loop = *
    lda (parm0),y ; get one byte
    beq txstr__done ; if it's zero, we're done
    jsr txchar ; transmit the byte
    iny ; on to the next byte
    bne txstr__loop ; will pretty much always branch, but the
                    ; possibility of falling through if someone passes
                    ; a string that's too long is nice
txstr__done = *
    rts ; and we can return now

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

