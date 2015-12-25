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

; For testing my 6502 based "VTJ-1" platform
; Interrupt driven LED blinking test.  This is the first test I've
; done with interrupts enabled.

; Planned functionality:
;   + gray counter on the LEDs
;   + with gradual change, one change per second
;   + thus, 32 seconds per full cycle
;   + meanwhile it'll transmit hexadecimal values from
;   a 24 bit Galois LFSR (out the serial port in slot 8 at 9600 baud, 8n2)

; Design:
;   + I'll use the timer interrupt, which runs at any of the various
;   standard baud rates
;   + I'll set it to 14,400 Hz
;       - 64 interrupt cycles per PWM pulse
;       - 225 PWM pulses per second / counter change
;   + The serial port part runs in non-interrupt mode, and is mainly
;   there for debugging - so I can determine if non-interrupt code
;   is running.

; In various error conditions, this program stops what it's doing and just
; writes single characters to the serial port:
;       'B' -- if it ran a BRK instruction
;       'I' -- if a bogus IRQ came in
;       'T' -- if the timer interrupt fired a second time during the
;           IRQ handler

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
BAUD_SETTING = $15 ; 9600 baud
TIMER_SETTING = $06 ; 14,400 Hz
INT_PER_PWM = 64 ; timer interrupts per PWM pulse (<= 255)
PWM_PER_CHG = 225 ; PWM pulses per timer change (<= 255)
LFSR_GEN_0 = $fb ; first byte of LFSR generator 0x1864CFB
LFSR_GEN_1 = $4c ; second byte of LFSR generator
LFSR_GEN_2 = $86 ; third byte of LFSR generator
USE_LEDS = 4 ; number of LEDs to use in the gray code

; Zero page addresses
lfsr = $00 ; 3 byte - Galois Linear Feedback Shift Register state
intctr = $03 ; 1 byte - interrupt cycle counter, see INT_PER_PWM
pwmctr = $04 ; 1 byte - PWM pulse counter, see PWM_PER_CHG
chgctr = $05 ; 1 byte - change counter driving the LEDs, counts seconds
fromgr = $06 ; 1 byte - "from" gray code value for LEDs
togr = $07 ; 1 byte - "to" gray code value for LEDs
dc = $08 ; 1 byte - duty cycle for half-lit LEDs

* = ROMBASE

ledlut = *
; look up table that matches values from 0 to PWM_PER_CHG-1, to values
; from 0 to INT_PER_PWM, which determine the duty cycle of the LED and
; thus how bright it ends up being.

; These were generated for 225 and 64, by the following formula:
;       y = (x / 224) ^ 3 * 63
  db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  db 0,0,0,0,0,0,0,0,0,0,0,0,1,1,1
  db 1,1,1,1,1,1,1,1,1,1,1,2,2,2,2
  db 2,2,2,2,2,2,2,3,3,3,3,3,3,3,3
  db 4,4,4,4,4,4,4,5,5,5,5,5,5,6,6
  db 6,6,6,7,7,7,7,7,8,8,8,8,8,9,9
  db 9,9,10,10,10,10,11,11,11,12,12,12,12,13,13
  db 13,14,14,14,15,15,15,16,16,16,17,17,17,18,18
  db 18,19,19,20,20,20,21,21,22,22,22,23,23,24,24
  db 25,25,26,26,27,27,28,28,29,29,30,30,31,31,32
  db 32,33,33,34,34,35,36,36,37,37,38,39,39,40,40
  db 41,42,42,43,44,44,45,46,46,47,48,49,49,50,51
  db 51,52,53,54,54,55,56,57,58,58,59,60,61,62,63

start = *
    ; Here's where the code will be when reset is done.

    ; disable interrupts until we've got it all set up
    sei

    ; initialize serial port
    lda #BAUD_SETTING
    sta SER_BASE+SER_TA ; set the baud rate
    lda #$30
    sta SER_BASE+SER_TB ; set to 8n2 (compatible with 8n1)

    ; initialize timer
    lda #TIMER_SETTING
    sta TIMER

    ; initialize interrupt controller: enable timer interrupt, which
    ; is slot 0's alpha IRQ.
    lda #1
    sta ICTL_ENA0
    ; the other 24 interrupt bits will already be zero after reset

    ; initialize state variables
    lda #1
    sta lfsr
    lda #0
    sta lfsr+1
    sta lfsr+2
    sta intctr
    sta pwmctr
    sta chgctr

    ; and enable interrupts, we're ready to go
    cli

main = *
    ; Main loop, of the non-interrupt part, the LFSR.

    ; Write the 24 bit LFSR value in hexadecimal.
    lda lfsr+2
    jsr hextx
    lda lfsr+1
    jsr hextx
    lda lfsr
    jsr hextx

    ; Write a space character, to delimit it from the next value.
    lda #' '
    jsr chartx

    ; Compute the next value of the LFSR.  It's a Galois LFSR (I never
    ; got the hang of the "usual" kind) so it's computed by shifting one
    ; bit to the left, and if the bit is shifted out is a 1, then XORing
    ; the generator.  ("EOR" is the 6502 mnemonic for the instruction which
    ; does the exclusive-or operation, which I prefer to call XOR.)
    asl lfsr
    rol lfsr+1
    rol lfsr+2
    bcc main ; nope, no XOR this time, we have our new LFSR value: repeat
    lda #LFSR_GEN_0
    eor lfsr
    sta lfsr
    lda #LFSR_GEN_1
    eor lfsr+1
    sta lfsr+1
    lda #LFSR_GEN_2
    eor lfsr+2
    sta lfsr+2
    jmp main ; we have our new LFSR value: repeat

hextx = *
    ; hextx() routine: Write to the serial port two hex digits, representing
    ; the single byte that's in the accumulator.  Clobbers it and the
    ; X register.

    pha ; save the byte value for later
    ror a ; now extracting the upper half byte
    ror a
    ror a
    ror a
    and #15
    tax ; and converting to hex
    lda hexlut, x
    jsr chartx ; and transmitting it
    pla ; and get the original byte value back
    and #15 ; and extract the lower half byte
    tax ; now converting to hex
    lda hexlut, x
    jsr chartx ; and transmitting it
    rts ; and done

chartx = *
    ; chartx() routine: Write the byte (character) value in the accumulator
    ; to the serial port.  Blocks until it can fit in the FIFO.  Preserves
    ; all registers except the flags.

    pha ; save the byte value

    ; Check for room on the output FIFO, which is indicated by slot 8's
    ; beta IRQ.  That's bit 0 of ICTL_PEB1.  We just keep checking it until
    ; it becomes 1.

chartx__loop = *
    lda ICTL_PEB1
    ror a ; extract bit 0 into carry
    bcc chartx__loop ; repeat if the bit wasn't 1

    ; Time to transmit the character.
    pla ; get the character
    sta SER_BASE+SER_TX ; and put it

    ; Done.
    rts

irq = *
    ; This is the interrupt handler.  It does pulse width modulation on
    ; the LEDs.

    ; This is my first time producing the sole code on a 6502 machine
    ; with interrupt handling (without somebody else's ROM code providing
    ; basic interface).  Figuring out what might be needed based on
    ; what's in the Commodore 64 ROM.
    ;       + the ROM's IRQ/BRK vector is $ff48
    ;       + it does: pha : txa : pha : tya : pha
    ;           obvious: saving the regular registers onto the stack
    ;       + then it does: tsx : lda $0104,x
    ;           it's getting a value off the stack
    ;           from what follows, it's probably the status word
    ;       + then: and #$10
    ;           testing a single bit of that
    ;           seems to be the "B" flag
    ;       + and from that it does one of two jumps
    ;           0 -> jmp ($0314) going to $ea31
    ;           1 -> jmp ($0316) going to $fe66

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
    beq irq__notbrk ; it's not BRK; good

    ; It's a BRK; not good.  Just indicate it on the LEDs and serial port
error_brk = *
    lda #25
    sta LEDOUT ; on the LEDs
    lda #'B'
    sta SER_BASE+SER_TX ; on the serial port: don't even check FIFO space
    bne error_brk ; just keep doing it

irq__notbrk = *
    ; This is the handler for a real interrupt request, not a BRK.
    lda ICTL_PEA0
    and #1 ; see if the timer interrupt is what woke us up
    bne timer__handle

    ; it wasn't the timer IRQ that woke us, we shouldn't have been woken
    ; up; indicate it on the LEDs and serial port
error_noirq = *
    lda #18
    sta LEDOUT ; on the LEDs
    lda #'I'
    sta SER_BASE+SER_TX ; on the serial port: don't even check FIFO space
    bne error_noirq ; just keep doing it

timer__handle = *
    ; Handle the timer interrupt:
    ;   + clear it so it can happen again at just the right time
    ;   + update the counters that control our use of the LEDs
    ;   + then update the LEDs
    ;   + then finish the interrupt handling

    ; Clear the timer interrupt.
    lda #TIMER_SETTING
    sta TIMER

    ; increment the counters
    inc intctr  ; interrupt counter increments, and maybe more
    lda intctr
    cmp #INT_PER_PWM ; does the interrupt counter wrap around?
    bne timer__figure ; no
    lda #0 ; the interrupt counter wraps around
    sta intctr

    inc pwmctr ; pulse counter increments, and maybe more
    lda pwmctr
    cmp #PWM_PER_CHG ; does the pulse counter wrap around?
    bne timer__figure ; no
    lda #0 ; the pulse counter wraps around
    sta pwmctr

    lda chgctr ; the change counter increments
    clc
    adc #1
MASK_LEDS = (1<<USE_LEDS)-1
    and #MASK_LEDS
    sta chgctr

timer__figure = *
    ; Now that the counters have been updated, it's time to figure
    ; out the gray code and duty cycle values, derived from them.
    ; Much of this effort is wasted: The values are recomputed every
    ; interrupt cycle whether they've changed or not.  But the code's
    ; simpler that way, and anyway, this is a blinkenlight program on
    ; a 75MHz 6502, it doesn't deserve that kind of carefulness.

    ; note: gray code can be computed as: (num >> 1) ^ num
    lda chgctr ; figure the "from" gray code
    lsr a
    eor chgctr
    sta fromgr
    lda chgctr ; figure the "to" gray code
    clc
    adc #1
    and #MASK_LEDS
    sta togr
    lsr a
    eor togr
    sta togr

    ; Now, by comparing fromgr & togr we can tell if an LED is gradually
    ; coming on or gradually turning off.  Then we figure out how far along
    ; it is, and what its duty cycle should be from that.
    lda fromgr
    cmp togr
    bpl timer__figure__offing
    ; fromgr < togr, meaning an LED is coming *on*
    ldx pwmctr
    lda ledlut,x
    sta dc
    jmp time__figure__gotdc
timer__figure__offing = *
    ; fromgr >= togr, meaning an LED is turning *off*
    ; so we take 'pwmctr' in reverse
    lda #PWM_PER_CHG-1
    sec
    sbc pwmctr ; PWM_PER_CHG - 1 - pwmctr
    tax
    lda ledlut,x
    sta dc

time__figure__gotdc = *
    ; Now use the three values we just calculated to figure
    ; out what the LEDs should be doing just now.  And go do it!

    ; fromgr and togr are the two possible results.  intctr < dc determines
    ; if we take the AND or the OR of them, which will differ in terms
    ; of the one LED which is in the process of changing.

    lda intctr
    cmp dc
    bpl time__figure__off
    ; intctr < dc: the half-lit LED will be *on* so use "or"
    lda fromgr
    ora togr
    jmp timer__setleds
time__figure__off = *
    ; intctr >= dc: the half-lit LED will be *off* so use "and"
    lda fromgr
    and togr

timer__setleds = *

    ; accumulator holds the value to store to the LEDs; store it
    sta LEDOUT

    ; Now we've updated the LEDs.  Time to finish up and let the main program
    ; run some more.

    ; At the start of 'timer' we cleared the timer interrupt.  If this
    ; code runs slower than it should, then the timer interrupt could
    ; have happened again.  In that case, this whole program will fail.
    ; That's not desirable, so rather than let it just happen, we'll
    ; halt everything and print 'T'.

    lda ICTL_PEA0 ; pending IRQs: slot 0-7 alpha
    ror a ; extract bit zero, the timer interrupt line
    bcs error_time ; branch if it interrupted again

    ; Now finally return from the interrupt, to the regularly scheduled
    ; program.
    pla
    tay
    pla
    tax
    pla
    rti

error_time = *
    ; print 'T' to indicate error; repeatedly.  Don't even check if the
    ; FIFO is empty or not, just keep filling it.
    lda #'T'
    sta SER_BASE+SER_TA
    bne error_time

hexlut = *
    ; Lookup table for hexadecimal characters.
    asc "0123456789abcdef"

* = $fffa
    dw start ; NMI vector (shouldn't run - no sources of NMI)
    dw start ; reset vector
    dw irq ; IRQ/BRK vector
