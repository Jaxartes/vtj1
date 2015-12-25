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

; For testing my 6502 based "VTJ-1" platform, in particular this is the
; second test of the video interface.  A pattern of 8x8 colored rectangular
; bullseyes fills the screen.

; Intended assembler: "crasm"

    cpu 6502
    code

; Basic system info
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
ICTL_ENA0 = $b000 ; interrupt control: enable slot 0-7 alpha IRQs
ICTL_ENA1 = $b001 ; interrupt control: enable slot 8-15 alpha IRQs
ICTL_ENB0 = $b008 ; interrupt control: enable slot 0-7 beta IRQs
ICTL_ENB1 = $b009 ; interrupt control: enable slot 8-15 beta IRQs
ICTL_PEA0 = $b040 ; interrupt control: pending slot 0-7 alpha IRQs
ICTL_PEA1 = $b041 ; interrupt control: pending slot 8-15 alpha IRQs
ICTL_PEB0 = $b048 ; interrupt control: pending slot 0-7 beta IRQs
ICTL_PEB1 = $b049 ; interrupt control: pending slot 8-15 beta IRQs
ICTL_PRI = $b080 ; interrupt control: priority encoding

V_SC0 = $b100 ; scan line control byte 0
              ;   &$0f - pixel row of font
              ;   &$10 - double width pixels & chars
              ;   &$20 - underline line
              ;   &$40 - enable output
              ;   &$80 - vertical sync value; 0 is pulse
V_SC1 = $b101 ; scan line control byte 1: text data address, 32 byte units
V_CTL = $b102 ; special commands and status
V_CTL_CLEAR = $07 ; clear sanity check failures

VM_CHS = $b700 ; channel selector
VM_LOG = $b701 ; logic function
VM_CTR_MET = $b704 ; 32-bit counter: times threshold was met
VM_CTR_IEN = $b708 ; 32-bit counter: clock cycles with interrupts enabled
VM_CTR_IDI = $b70c ; 32-bit counter: clock cycles with interrupts disabled
VM_THRESH = $b710 ; threshold

TEXTRAM = $8000 ; start of 8kB text RAM
FONTRAM = $a000 ; start of 4kB font RAM

BAUDCODE = $15 ; code for 9600 baud
TXMODE = $30 ; code for 8n2, compatible with 8n1
RXMODE = $20 ; code for 8n1

; Zero page addresses
irqtmp = $00 ; 2 byte - a pointer that can only be used in interrupt mode
scanline = $02 ; 2 bytes - scan line counter
virqctr = $04 ; 4 bytes - number of times the video IRQ handler is run
uaddr = $08 ; 2 bytes - "user" address
inarg = $0a ; 1 byte - input argument byte from the user
scanlinetmp = $0b ; 2 bytes - temporary copy of scanline
virqctrtmp = $0d ; 4 bytes - temporary copy of virqctr
addr0 = $11 ; 2 bytes - pointer to data
initrc = $13 ; 1 byte - row color, used only during initialization
initcc = $14 ; 1 byte - column color, used only during initialization
sldbg = $15 ; 4 bytes - scanline, V_SC0, V_SC1 set during visible part
sldbg2 = $19 ; 4 bytes - copies of those for display
verrclr = $1a ; 1 byte - written every video IRQ to clear errors

; Other addresses
doublerows = $700 ; 60 bytes - a $10 for any double-width text rows

; And now the program.

* = $c000

cmd_w = *
    ; cmd_w: The 'w' command will loop for an estimated 1287*inarg clock
    ; cycles (plus a handful more) with interrupts disabled.
    jsr txchar ; echo the 'w' command character
    ldy #0 ; inner counter
    sei ; disable interrupts
cmd_w__loop = *
    dey
    bne cmd_w__loop
    dec inarg
    bne cmd_w__loop
    cli ; enable interrupts
    jmp main

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
    ; check for availability of a character or exception in
    ; the serial device's buffer, which is indicated by its alpha IRQ
    lda ICTL_PEA1
    ror a ; get bit 0 into carry
    bcc rxchar ; not ready

    ; it's ready; receive a byte or exception
    lda SER_BASE+SER_ST ; serial status byte
    ror a ; get bit 0 into carry
    lda SER_BASE+SER_RX ; byte or exception received
    sta SER_BASE+SER_RX ; remove it from the buffer to make room for another

    rts ; done

rasters = *
    ; Lookup table for use in computing V_SC1 values.  It maps each integer
    ; 0-255, to x mod 15 times 5, which is the row to use for that scan
    ; line.
    db 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70
    db 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70
    db 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70
    db 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70
    db 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70
    db 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70
    db 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70
    db 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70
    db 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70
    db 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70
    db 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70
    db 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70
    db 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70
    db 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70
    db 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70
    db 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70
    db 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70
    db 0

start = *
    ; disable interrupts for now
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
    sta scanline
    sta scanline+1
    sta virqctr
    sta virqctr+1
    sta virqctr+2
    sta virqctr+3

    ; blank the LEDs
    lda #0
    sta LEDOUT

    ; initialize our "font", it has a single 8x8 character
    ldx #0
    lda #FONTRAM&255 ; record our address
    sta irqtmp
    lda #FONTRAM>>8
    sta irqtmp+1
    lda #$ff ; first row of character
    sta (irqtmp,x)
    lda #$81 ; second & third rows of character
    inc irqtmp+1
    sta (irqtmp,x)
    inc irqtmp+1
    sta (irqtmp,x)
    lda #$99 ; fourth & fifth rows of character
    inc irqtmp+1
    sta (irqtmp,x)
    inc irqtmp+1
    sta (irqtmp,x)
    lda #$81 ; sixth & seventh rows of character
    inc irqtmp+1
    sta (irqtmp,x)
    inc irqtmp+1
    sta (irqtmp,x)
    lda #$ff ; eighth row of character
    inc irqtmp+1
    sta (irqtmp,x)

    ; initialize the first 15 rows of text RAM (2400 bytes) with a pattern
    ; that uses always character zero, but with different foreground
    ; color and foreground intensity
    ldy #0 ; will use as index; will be zero or one
    lda #TEXTRAM&255 ; address where it goes
    sta irqtmp
    lda #TEXTRAM>>8
    sta irqtmp+1
    lda #1 ; row color counter, it'll range 1-15
    sta initrc
start__trloop = *
    ; starting a row
    lda initrc ; column color counter, it'll range 1-15
    sta initcc
    ldx #0 ; column counter, it'll range 0-79
start__tcloop = *
    ; doing a character cell pointed to by irqtmp
    lda #0 ; character code zero
    sta (irqtmp),y
    lda initcc ; foreground color & brightness
    iny
    sta (irqtmp),y
    dey
    ; now move on to the next column
    clc ; increment column color counter
    adc #1
    cmp #16
    bmi start__ccnowrap ; and see about wraparound to 1
    lda #1
start__ccnowrap = *
    sta initcc
    lda irqtmp ; add 2 to character pointer
    clc
    adc #2
    sta irqtmp
    lda irqtmp+1
    adc #0
    sta irqtmp+1
    inx ; increment column counter
    txa
    cmp #80 ; and see if the last column is done
    bmi start__tcloop
    ; end of column
    inc initrc ; next row color
    lda initrc
    cmp #16 ; have we done all of them?
    bmi start__trloop ; branch if not
    ; text memory has been initialized

    ; initialize 'doublerows' to zero
    ldx #59
    lda #0
start__doublerows_loop = *
    sta doublerows,x
    dex
    bpl start__doublerows_loop

    ; initialize control flags
    lda #V_CTL_CLEAR
    sta verrclr

    ; initialize interrupt handler(s)
    lda #2 ; enabling slot 1's alpha interrupt (video row)
    sta ICTL_ENA0
    ; the other 24 interrupt bits will already by zero after reset

    ; enable interrupts
    cli

cmd_a = *
    ; 'a' - store 'inarg' to 'uaddr'
    jsr txchar ; echo the command
    lda inarg ; copy the value
    sta uaddr
    jmp main ; next command

cmd_b = *
    ; 'b' - store 'inarg' to 'uaddr+1'
    jsr txchar ; echo the command
    lda inarg ; copy the value
    sta uaddr+1
    jmp main ; next command

cmd_c = *
    ; 'c' - store 'inarg' to 'VM_CHS'
    jsr txchar ; echo the command
    lda inarg ; copy the value
    sta VM_CHS
    jmp main ; next command

cmd_l = *
    ; 'l' - store 'inarg' to 'VM_LOG'
    jsr txchar ; echo the command
    lda inarg ; copy the value
    sta VM_LOG
    jmp main ; next command

cmd_r = *
    ; 'r' - report the four counters & scan line in hex
    sei ; don't let interrupts happen while we retrieve the counters
    sta VM_CTR_MET ; writing will latch values into the three VM_CTR_* counters
    lda scanline
    sta scanlinetmp
    lda scanline+1
    sta scanlinetmp+1
    lda virqctr
    sta virqctrtmp
    lda virqctr+1
    sta virqctrtmp+1
    lda virqctr+2
    sta virqctrtmp+2
    lda virqctr+3
    sta virqctrtmp+3
    cli ; we can let interrupts happen now
    jsr txnewline ; put space around our output
    ; now we display our counters
    lda #'R'
    jsr txchar
    lda #virqctrtmp&255
    sta addr0
    lda #virqctrtmp>>8
    sta addr0+1
    jsr txhex4
    lda #','
    jsr txchar
    lda #'S'
    jsr txchar
    lda scanlinetmp+1
    jsr txhex
    lda scanlinetmp
    jsr txhex
    lda #','
    jsr txchar
    lda #'M'
    jsr txchar
    lda #VM_CTR_MET&255
    sta addr0
    lda #VM_CTR_MET>>8
    sta addr0+1
    jsr txhex4
    lda #','
    jsr txchar
    lda #'E'
    jsr txchar
    lda #VM_CTR_IEN&255
    sta addr0
    lda #VM_CTR_IEN>>8
    sta addr0+1
    jsr txhex4
    lda #','
    jsr txchar
    lda #'D'
    jsr txchar
    lda #VM_CTR_IDI&255
    sta addr0
    lda #VM_CTR_IDI>>8
    sta addr0+1
    jsr txhex4
    jsr txnewline
    jmp main

cmd_excl = *
    ; '!' - store 'inarg' to '*uaddr'
    jsr txchar ; echo the command
    lda inarg ; get the value
    ldx #0
    sta (uaddr,x) ; write it to *uaddr
    jmp main ; next command

cmd_ques = *
    ; '?' - read '*uaddr' and report on it in hex
    lda #'(' ; print address, contents, and decoration
    jsr txchar
    lda uaddr+1
    jsr txhex
    lda uaddr
    jsr txhex
    lda #':'
    jsr txchar
    ldx #0
    lda (uaddr,x)
    jsr txhex
    lda #')'
    jsr txchar
    jmp main ; next command

cmd_ll = *
    ; 'L' - set LEDs from 'inaddr'
    jsr txchar ; echo the command
    lda inarg ; copy the value
    sta LEDOUT
    jmp main ; next command

cmd_ss = *
    ; 'S' - display what's in sldbg
    ldy #0 ; copy sldbg to sldbg2 to have all snapshotted in time
    sei
    lda sldbg,y
    sta sldbg2,y
    iny
    lda sldbg,y
    sta sldbg2,y
    iny
    lda sldbg,y
    sta sldbg2,y
    iny
    lda sldbg,y
    sta sldbg2,y
    cli
    lda #'[' ; now write it out
    jsr txchar
    lda sldbg2+1
    jsr txhex
    lda sldbg2
    jsr txhex
    lda sldbg2+2
    jsr txhex
    lda sldbg2+3
    jsr txhex
    lda #']'
    jsr txchar
    jmp main ; next command

cmd_at = *
    ; '@' - do nothing but blank 'inaddr'
    jsr txchar ; echo the command
    jmp main ; next command

cmd_mm = *
    ; 'M' - go through text RAM and change colors to white on black
    jsr txchar ; echo the character
    lda #TEXTRAM&255 ; low byte of start address
    sta addr0
    lda #TEXTRAM>>8 ; high byte of start address
    sta addr0+1
    ldx #10 ; 256 byte pages to process (32 in all, but only 10 interesting)
    ldy #0 ; byte counter within pages
cmd_mm__loop = *
    iny ; skip the character code, go on to the attribute byte
    lda #7 ; white on black
    sta (addr0),y ; store the attribute byte
    ; and move on to next byte, in this or another page, if any
    iny
    bne cmd_mm__loop ; more bytes in this page
    inc addr0+1 ; move on to the next page
    dex ; and count it
    bpl cmd_mm__loop ; loop while there are still pages
    jmp main ; no more pages: return

cmd_a_jmp       jmp cmd_a
cmd_b_jmp       jmp cmd_b
cmd_c_jmp       jmp cmd_c
cmd_l_jmp       jmp cmd_l
cmd_r_jmp       jmp cmd_r
cmd_excl_jmp    jmp cmd_excl
cmd_ques_jmp    jmp cmd_ques
cmd_ll_jmp      jmp cmd_ll
cmd_ss_jmp      jmp cmd_ss
cmd_at_jmp      jmp cmd_at
cmd_w_jmp       jmp cmd_w
cmd_mm_jmp      jmp cmd_mm

main = *
    lda #0 ; clear the value for the next command
    sta inarg

main__noclear = *
    jsr rxchar ; character from serial port
    cmp #'a' ; deal with different commands
    beq cmd_a_jmp
    cmp #'b'
    beq cmd_b_jmp
    cmp #'c'
    beq cmd_c_jmp
    cmp #'l'
    beq cmd_l_jmp
    cmp #'r'
    beq cmd_r_jmp
    cmp #'!'
    beq cmd_excl_jmp
    cmp #'?'
    beq cmd_ques_jmp
    cmp #'L'
    beq cmd_ll_jmp
    cmp #'S'
    beq cmd_ss_jmp
    cmp #'w'
    beq cmd_w_jmp
    cmp #'@'
    beq cmd_at_jmp
    cmp #'M'
    beq cmd_mm_jmp
    sec
    sbc #'0' ; convert to digit value, assuming it's a digit
    bmi main__noclear ; it's not a digit
    cmp #10
    bpl main__noclear ; it's not a digit
    ; it's a digit; so do: inarg = inarg * 10 + digit
    pha ; stash the digit
    asl inarg ; inarg * 2
    lda inarg
    asl a
    asl a ; inarg * 8
    clc
    adc inarg ; inarg * 10
    sta inarg
    pla ; get the digit back
    pha ; stash it again
    clc ; and add it to the number
    adc inarg
    sta inarg
    pla ; get the digit back
    clc
    adc #'0' ; convert it back to ASCII
    jsr txchar ; and echo it
    jmp main__noclear ; and do the next command character

hextbl = *
    ; table of hex digits
    asc "0123456789abcdef"

txnewline = *
    ; txnewline(): transmit carriage return & line feed
    lda #13
    jsr txchar
    lda #10
    jsr txchar
    rts

txstr = *
    ; txstr(): Transmit the null-terminated string pointed to by 'addr0'
    ldy #0 ; pointer to individual bytes of the string
txstr__loop = *
    lda (addr0),y ; get one byte
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

txhex4 = *
    ; txhex4(): Output a 4-byte hexadecimal value pointed to by *addr0
    ldy #3
    lda (addr0),y
    jsr txhex
    dey
    lda (addr0),y
    jsr txhex
    dey
    lda (addr0),y
    jsr txhex
    dey
    lda (addr0),y
    jsr txhex
    rts

panic = *
    ; panic(): simple function for system crash.  Sets LEDs and prints a
    ; character on the output serial port.
    sei ; don't be interrupted
    sta LEDOUT ; put it on LEDs
    sta SER_BASE+SER_TX ; put it in output FIFO even if FIFO is full
    jmp panic

irqbrk = *
    ; This is the interrupt handler.

    ; save registers
    pha
    txa
    pha
    tya
    pha

    ; check for BRK
    tsx
    lda $0104,x
    and #$10
    beq irq

    ; this is BRK, which shouldn't have happened
    lda #'B'
    jmp panic

irq__done = *
    ; Done handling interrupts, now return from the interrupt handler.
    pla
    tay
    pla
    tax
    pla
    rti

irq = *
    ; We got an interrupt request (IRQ).  Figure out which one to handle.
    lda ICTL_PRI ; what's the highest priority interrupt?
    bmi irq__done
    asl a ; look up the IRQ number in the dispatch table
    tax
    lda irq_tbl,x
    sta irqtmp
    lda irq_tbl+1,x
    sta irqtmp+1
    jmp (irqtmp)

irq_tbl = *
    ; IRQ dispatch table.  Has 32 entries, for the 32 IRQs supported by
    ; the interrupt controller.  Each interrupt handler should end
    ; with 'jmp irq' to handle any more pending interrupts.

    dw unimplemented_irq ; slot 0 alpha interrupt (timer)
    dw video_irq         ; slot 1 alpha interrupt (video, row data)
    dw unimplemented_irq ; slot 2 alpha interrupt (GPIO, unused IRQ)
    dw unimplemented_irq ; slot 3 alpha interrupt (empty slot)
    dw unimplemented_irq ; slot 4 alpha interrupt (empty slot)
    dw unimplemented_irq ; slot 5 alpha interrupt (empty slot)
    dw unimplemented_irq ; slot 6 alpha interrupt (empty slot)
    dw unimplemented_irq ; slot 7 alpha interrupt (timing tester, unused IRQ)

    dw unimplemented_irq ; slot 8 alpha interrupt (serial port, RX possible)
    dw unimplemented_irq ; slot 9 alpha interrupt (empty slot)
    dw unimplemented_irq ; slot 10 alpha interrupt (empty slot)
    dw unimplemented_irq ; slot 11 alpha interrupt (empty slot)
    dw unimplemented_irq ; slot 12 alpha interrupt (empty slot)
    dw unimplemented_irq ; slot 13 alpha interrupt (empty slot)
    dw unimplemented_irq ; slot 14 alpha interrupt (empty slot)
    dw unimplemented_irq ; slot 15 alpha interrupt (empty slot)

    dw unimplemented_irq ; slot 0 beta interrupt (system control, unused IRQ)
    dw unimplemented_irq ; slot 1 beta interrupt (video, unused IRQ)
    dw unimplemented_irq ; slot 2 beta interrupt (GPIO, unused IRQ)
    dw unimplemented_irq ; slot 3 beta interrupt (empty slot)
    dw unimplemented_irq ; slot 4 beta interrupt (empty slot)
    dw unimplemented_irq ; slot 5 beta interrupt (empty slot)
    dw unimplemented_irq ; slot 6 beta interrupt (empty slot)
    dw unimplemented_irq ; slot 7 beta interrupt (timing tester, unused IRQ)

    dw unimplemented_irq ; slot 8 beta interrupt (serial port, TX possible)
    dw unimplemented_irq ; slot 9 beta interrupt (empty slot)
    dw unimplemented_irq ; slot 10 beta interrupt (empty slot)
    dw unimplemented_irq ; slot 11 beta interrupt (empty slot)
    dw unimplemented_irq ; slot 12 beta interrupt (empty slot)
    dw unimplemented_irq ; slot 13 beta interrupt (empty slot)
    dw unimplemented_irq ; slot 14 beta interrupt (empty slot)
    dw unimplemented_irq ; slot 15 beta interrupt (empty slot)

unimplemented_irq = *
    ; Handler for an IRQ that doesn't have a handler; it shouldn't have been
    ; enabled and it shouldn't have happened.
    lda #'J'
    jmp panic

video_irq = *
    ; Handler for the video IRQ, which gets called when it's time to
    ; set up another row's information in V_SC0 & V_SC1.

    ; In the real product, this handler might go through a linked
    ; list of text rows, and apply their settings.  But for now, it's
    ; simplified for testing purposes.  There're only two raster lines
    ; printed, we alternate them.

    ; count the number of times this IRQ handler runs
    lda virqctr
    clc
    adc #1
    sta virqctr
    lda virqctr+1
    adc #0
    sta virqctr+1
    lda virqctr+2
    adc #0
    sta virqctr+2
    lda virqctr+3
    adc #0
    sta virqctr+3

    ; clear any sanity check failures
    lda verrclr ; normally this == V_CTL_CLEAR
    sta V_CTL

    ; now compute video timings for the current 'scanline' value
    ;       0-479 -- visible area
    ;           even & odd lines have different values
    ;       480-489 -- front porch
    ;           not visible
    ;       490-491 -- sync pulse
    ;           goes low for these two lines
    ;       492-524 -- back porch
    ;           not visible
    lda scanline+1
    beq video_visible ; scanline < 256
    cmp #2
    bpl video_porch ; 512 <= scanline
    lda scanline
    bpl video_visible ; 256 <= scanline <= 383
    cmp #224
    bmi video_visible ; 384 <= scanline < 480
    cmp #234
    bmi video_porch ; 480 <= scanline < 490
    cmp #236
    bmi video_sync ; 490 <= scanline < 492
video_porch = *
    ; this row is in the front or back porch
    lda #$80 ; not in sync pulse; but no visible output either
    sta V_SC0
    ; don't bother setting text address, no-one can see it
    lda scanline
    cmp #12 ; scan line 524 is followed by zero
    beq video_newframe
    jmp video_next
video_sync = *
    ; this row is in the sync pulse
    lda #$00
    sta V_SC0
    ; don't bother setting text address, no-one can see it
    jmp video_next
video_visible = *
    ; This row is in the visible area.
    ; V_SC0 gets:
    ;   &$0f - scanline & $07 - vertical position within font
    ;   &$10 - double width, taken from 'doublerows'
    ;   &$20 - zero, no underline
    ;   &$40 - one, enable output
    ;   &$80 - one, not in sync pulse
    ; V_SC1 gets: Scan line, divided by 8, modulo 15, times 5.
    ; Assisted by table 'rasters'.
    lda scanline + 1; as part of computing V_SC0: get text row number (into X)
    ror a
    lda scanline
    ror a
    lsr a
    lsr a
    tax
    lda scanline ; as part of computing V_SC0: get font row number
    and #7
    ora #$c0
    clc
    adc doublerows,x ; usually zero, sometimes sixteen ($10)
    sta V_SC0
    sta sldbg+2 ; and also store it for debugging
    lda scanline+1 ; now for V_SC1, first dividing scanline by 8
    ror a
    lda scanline
    ror a ; divided 9 bits of scan line, by 2
    lsr a ; dividing that by 2
    lsr a ; now accumulator holds scanline divided by 8
    tax ; which we'll use for lookup in 'rasters'
    lda rasters,x
    sta V_SC1
    sta sldbg+3 ; and also store it for debugging
    lda scanline ; and store scanline for debugging too
    sta sldbg
    lda scanline+1
    sta sldbg+1
video_next = *
    ; now increment scanline - the value we'll use the next time
    ; this IRQ handler gets called
    lda scanline
    clc
    adc #1
    sta scanline
    lda scanline+1
    adc #0
    sta scanline+1
    jmp irq ; handle any more IRQs that are pending
video_newframe = *
    ; next scan line: zero
    lda #0
    sta scanline
    sta scanline+1
    jmp irq ; handle any more IRQs that are pending
    
* = $fffc
    dw start ; reset vector
    dw irqbrk ; interrupt vector

