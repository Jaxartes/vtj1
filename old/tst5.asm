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

; For testing my 6502 based "VTJ-1" platform: test serial RX.  Contains
; some infrastructure for handling IRQs, but doesn't actually have any
; IRQs to handle.

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
ICTL_PEPR = $b080 ; interrupt control: highest priority pending IRQ
TIMER = $b0c0 ; timer built into the system controller on slot zero

; other constants and addresses
MYBASE = $fc00 ; start of this program's part of ROM

; Zero page addresses
; System services use addresses from $00 up.  Loaded programs, and the
; bootloader itself, should allocate addresses from $ff downward, so they
; don't interfere.
systmp = $00 ; temporary use: 4 bytes
baudmode = $04 ; speed mode (0, 1, 2) cycles upon reset - 1200/9600/115.2k baud
baudcode = $05 ; serial port speed setting value, derived from spmode
parm1 = $06 ; two byte parameter pointer, used by txstr()

prgtmp = $fb ; 4 bytes - temporary use
number = $ff ; 1 byte - number read from the user

; And now the program.

* = MYBASE

main = *
    ; This is the main program, the part that runs with interrupts enabled
    ; (mostly).  Eventually it's going to be a bootloader or something fancy
    ; like that, but for now it's just a test program for serial port RX/TX.
    ; 
    ; It reads lines from the input and processes them.  For each line it
    ; prints a prompt, "ENTER>".  In reading the line it takes the following
    ; characters as having meaning:
    ;   digits 0-9 -- these make up a decimal number
    ;   letter 'l' or 'L' -- put the preceding number on the LEDs
    ;   letter 'c' or 'C' -- take the preceding number as a character
    ;                        code and print it
    ;   letter 'n' or 'N' -- perform bitwise NOT on the number
    ;   other - results in '?' being printed
    ;   CR -- ignored
    ;   LF -- ends the line

    ; Handling a line.  First, print the prompt
    lda #prompt_string&$ff
    sta parm1
    lda #prompt_string>>8
    sta parm1+1
    jsr txstr

    ; Now, initialize the number to zero
    lda #0
    sta number

    ; Now process characters one at a time
main__charloop = *
    jsr rxchar ; get a character
    cmp #13
    beq main ; CR - ignore, but print the prompt
    cmp #10
    beq main__charloop ; LF - ignore
    cmp #48
    bmi main__notdigit
    cmp #58
    bpl main__notdigit
    ; it's a digit - put it into 'number'
    ; by multiplying the number by 10 + adding the digit
    sec
    sbc #48 ; convert from ASCII to digit value
    pha ; store it for later
    asl number ; number*2
    lda number
    asl a ; number*4
    asl a ; number*8
    clc
    adc number ; number*10
    sta number
    clc
    pla ; get the digit value back
    adc number ; number*10+digitval
    sta number
    ; that digit's been handled, do another character
    jmp main__charloop

main__notdigit = *
    ; so, the character is not a digit; assume it's a letter, and
    ; convert it to lower case.
    ora #$20
    ; and see what letter it is
    cmp #'l'
    beq main__leds
    cmp #'c'
    beq main__char
    cmp #'n'
    beq main__not
    ; unknown letter: just print '?' and get another
    lda #'?'
    jsr txchar
    jmp main__charloop

main__leds = *
    ; display the current number on the LEDs
    lda number
    sta LEDOUT
main__blankrep = *
    lda #0 ; and blank the number
    sta number
    jmp main__charloop ; and do another number

main__char = *
    ; write out the current number as a character
    lda number
    jsr txchar
    jmp main__blankrep ; and blank number & do another char

main__not = *
    ; flip every bit of the number
    lda number
    eor #$ff
    sta number
    jmp main__charloop ; and do another char

prompt_string = *
    db 13, 10
    asc "ENTER"
    db '>'|$80

baud_tbl = *
    ; baud rates to go with the three baudmode values
    db $18 ; 1200 baud
    db $15 ; 9600 baud
    db $03 ; 115.2 kbaud

baud_led_tbl = *
    ; LED patterns to indicate the current baud rates
    db 1 ; on/off/off indicating 1200 baud
    db 3 ; on/on/off indicating 9600 baud
    db 5 ; on/off/on indicating 115.2 kbaud

reset = *
    ; Here's where the code will be when reset is done.

    ; disable interrupts until we've got it all set up
    sei

    ; initialize devices
    jsr serial_init ; initialize the serial port

    ; enable interrupts
    cli

    ; run main program
    jmp main

serial_init = *
    ; initialize the serial port on slot 8

    ; figure out what speed we're going to use; it cycles with every reset
    ; It's in 'baudmode', value 0 - 2.
    inc baudmode
    lda baudmode
    cmp #3
    bmi serial_init__got_baudmode ; no need to wrap around
    lda #0 ; wrap around to zero
    sta baudmode
serial_init__got_baudmode = *
    tax
    lda baud_tbl,x ; look up the corresponding baud speed code
    sta baudcode
    lda baud_led_tbl,x ; and the LED pattern to let the user know about it
    sta LEDOUT

    ; initialize the serial port device
    lda baudcode
    sta SER_BASE+SER_TA ; set the baud rate
    sta SER_BASE+SER_RA ; set the baud rate
    lda #$30
    sta SER_BASE+SER_TB ; set to 8n2 (compatible with 8n1)
    lda #$20
    sta SER_BASE+SER_RB ; set to 8n1

    ; Enable the serial port's interrupts.  For now, the serial port
    ; handling isn't interrupt driven, but in the future it might be.
    ;; lda ICTL_ENA1
    ;; eor #1
    ;; sta ICTL_ENA1 ; alpha interrupt: when RX possible 
    ;; lda ICTL_ENB1
    ;; eor #1
    ;; sta ICTL_ENB1 ; beta interrupt: when TX possible

    rts

txstr = *
    ; txstr()
    ; Routine to transmit a string on the serial port in slot 8.
    ; Takes a pointer to the string in 'parm1'; expects the final character
    ; if the string to be identified by a high bit of '1'.
    ; Messes with registers A & Y; leaves X alone.
    ldy #0
txstr__loop = *
    lda (parm1),y ; read the next character
    bmi txstr__loop_last ; branch if it's the last character
    jsr txchar ; transmit the character
    iny ; and move on to the next one
    bne txstr__loop ; (nearly) always takes the branch
txstr__loop_last = *
    ; the accumulator holds the last character, with its high bit set
    and #$7f ; transmit it without its high bit set
    jsr txchar
    rts ; and done

txchar = *
    ; txchar()
    ; Routine to transmit a byte on the serial port in slot 8.
    ;
    ; Call it with the byte in the accumulator; doesn't alter any
    ; registers.
    ;
    ; In the future it might have a buffer and an interrupt handler, but
    ; for now it just waits until it is safe to go.

    pha ; stash the byte value for later

txchar__loop = *
    ; check for availability of space in the serial device's buffer
    ; which is indicated by its beta IRQ
    lda ICTL_PEB1
    ror a ; get bit 0 into carry
    bcc txchar__loop ; not ready

    ; it's ready; write the byte
    pla
    sta SER_BASE+SER_TX

    rts ; done

rxchar = *
    ; rxchar()
    ; Routine to receive a byte on the serial port in slot 8.
    ;
    ; Returns with either:
    ;       . the received byte value, in the accumulator; carry flag is clear
    ;       . some exception value, in the accumulator; carry flag is set
    ;
    ; Doesn't mangle any other registers.
    ;
    ; In the future it might have a buffer and an interrupt handler, but
    ; for now it just waits until it is safe to go.

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
    
irq_or_brk = *
    ; Handler for two interrupts: IRQ and BRK

    ; save registers (based on Commodore 64 ROM)
    pha
    txa
    pha
    tya
    pha

    ; check for BRK (based on Commodore 64 ROM)
    tsx
    lda $0104,x
    and #$10
    beq irq ; it's IRQ, and not BRK

    ; This is the interrupt handler for BRK.
    lda #'B' ; for now unsupported
    jmp panic

irq = *
    ; This is the handler for an IRQ.  Figures out what the highest
    ; priority pending interrupt is, and calls its handler.
    lda ICTL_PEPR
    bpl irq__really

    ; Turns out no IRQ was waiting after all.  Return from the
    ; interrupt handler. (based on Commodore 64 ROM)
    pla
    tay
    pla
    tax
    pla
    rti

irq__really = *
    ; Accumulator register has the number (0-31) of the IRQ that's pending.
    asl a
    tax
    lda irq_tbl,x
    sta systmp
    lda irq_tbl+1,x
    sta systmp+1
    jmp (systmp)

irq_tbl = *
    ; IRQ dispatch table.  Has 32 entries, for the 32 IRQs supported by the
    ; interrupt controller.  Each interrupt handler should end with
    ; 'jmp irq' to handle any more pending interrupts.

    dw unimplemented_irq ; slot 0 alpha interrupt (timer)
    dw unimplemented_irq ; slot 1 alpha interrupt (video, row data)
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

nmi = *
    ; Handler for NMI, which this architecture shouldn't ever generate.
    lda #'N'
    jmp panic

panic = *
    ; panic(): Call this function when you have an unrecoverable fatal
    ; error.  You can call it with interrupts disabled or enabled.
    ; It takes the error code in the accumulator, and displays it on
    ; the LEDs and sends it out the serial port.

    ; It never returns.

    sei ; disable interrupts
    pha ; save the accumulator (panic code) for later
    lda LEDOUT ; set the final two LEDs
    ora #$18
    sta LEDOUT
    lda baudcode ; reinitialize the serial port (TX only)
    sta SER_BASE+SER_TA
    lda #$30
    sta SER_BASE+SER_TB ; 8n2
    clc
    pla ; retrieve the saved accumulator (panic code)

panic__loop = *
    sta SER_BASE+SER_TX ; put it in the output FIFO whether or not it's empty
    bcc panic__loop ; branch always taken

* = $fffa
    dw nmi ; NMI vector (shouldn't run - no sources of NMI)
    dw reset ; reset vector
    dw irq ; IRQ/BRK vector

