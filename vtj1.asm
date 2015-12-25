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

; Jeremy Dilatush / 2015
; VTJ-1 is a text terminal, intended to be roughly VT-100 compatible.
; This is the software code for it, which runs on top of VTJ-1's Verilog
; code on an FPGA.

; For plans, status, and a lot more information, see vtj1-overview.txt and
; vtj1-design.txt.

; Intended assembler: "crasm"

; YYY bcc/bcs can be used for unsigned comparison (bcc = blt, bcs = bge)
; and I haven't been doing that; sometimes my workaround for it is
; awkward.

; Current status & plans: see notes

;; ;; ;; ;; Basic setup

; Include vtj1-config.asm: defines that control the functionality of this
; software at build time.  Derived from vtj1-config.txt by vtj1-config-gen.tcl.
INCLUDE vtj1-config.asm

    cpu 6502
    code

IF ENABLE_BIGROM
* = $c000
ELSE
* = $e000
ENDC

;; ;; ;; ;; Constants; mostly memory addresses

; Basic system info
LEDOUT = $b280 ; write LED state
BTNIN = $b281 ; read button state
SYSSFC = $b282 ; system special features control
    SYSSFC_BEEP = 15 ; code to start a beep
    SYSSFC_NOBEEP = 25 ; code to stop a beep
SER_BASE = $b800 ; base of serial port registers
    SER_TX = $1 ; register to transmit a byte
    SER_RX = $2 ; register to receive a byte
    SER_ST = $3 ; register to query device status
    SER_TA = $4 ; transmit control byte A
    SER_TB = $5 ; transmit control byte B
    SER_RA = $6 ; receive control byte A
    SER_RB = $7 ; receive control byte B
PS2A_BASE = $bc00 ; base of PS/2 port A registers
    PS2_TX = $1 ; offset of register to transmit a byte
    PS2_RX = $2 ; to receive a byte
    PS2_EXC = $3 ; register holding exception bits
        PS2_EXC_TXBADACK = $20 ; bit in PS2_EXC: TX bad ACK from device
        PS2_EXC_RXOFLOW = $40 ; bit in PS2_EXC: RX overflow
        PS2_EXC_RXPARITY = $80 ; bit in PS2_EXC: RX parity or framing error
ICTL_ENA0 = $b000 ; interrupt control: enable slot 0-7 alpha IRQs
ICTL_ENA1 = $b001 ; interrupt control: enable slot 8-15 alpha IRQs
ICTL_ENB0 = $b008 ; interrupt control: enable slot 0-7 beta IRQs
ICTL_ENB1 = $b009 ; interrupt control: enable slot 8-15 beta IRQs
ICTL_PEA0 = $b040 ; interrupt control: pending slot 0-7 alpha IRQs
ICTL_PEA1 = $b041 ; interrupt control: pending slot 8-15 alpha IRQs
ICTL_PEB0 = $b048 ; interrupt control: pending slot 0-7 beta IRQs
ICTL_PEB1 = $b049 ; interrupt control: pending slot 8-15 beta IRQs
ICTL_PRI = $b080 ; interrupt control: priority encoding

SCREEN_WIDTH = 80 ; characters in a row
CURSOR_FRAMES = 30 ; video frames per cursor blinking cycle, over two
                   ; 30 is the right value for 60Hz video, one-second cursor
; V_* are video registers
V_SC0 = $b100 ; scan line control byte 0
              ;   &$0f - pixel row of font
              ;   &$10 - double width pixels & chars
              ;   &$20 - underline line
              ;   &$40 - enable output
              ;   &$80 - vertical sync value; 0 is pulse
V_SC1 = $b101 ; scan line control byte 1: text data address, 32 byte units
V_CTL = $b102 ; special commands, status, and additional flags
V_CTL_CLEAR = $07 ; clear sanity check failures
V_CTL_SETF = $0e ; set/clear inverse video flag
V_CTL_SETF2 = $0f ; set/clear second inverse video flag (used for visbell)
V_CTL_FINV = $10 ; inverse video flag
V_CTL_FINV2 = $20 ; second inverse video flag (used for visbell)

; VM_* are registers in the "video timing tester" optional component
VM_CHS = $b700 ; channel selector
VM_LOG = $b701 ; logic function
VM_CTR_MET = $b704 ; 32-bit counter: times threshold was met
VM_CTR_IEN = $b708 ; 32-bit counter: clock cycles with interrupts enabled
VM_CTR_IDI = $b70c ; 32-bit counter: clock cycles with interrupts disabled
VM_THRESH = $b710 ; threshold

TEXTRAM = $8000 ; start of 8kB text RAM
FONTRAM = $a000 ; start of 2kB (or 4kB) font RAM

TXMODE = $30 ; code for 8n2, compatible with 8n1
RXMODE = $20 ; code for 8n1

FONT_INFO_CHAR = 127 ; character code that instead of holding an actual
                     ; glyph, holds font information
CHECKERBOARD_GLYPH = 2 ; character that has a "checkerboard shape" in our font
XON_CHAR = 17 ; character code for XON
XOFF_CHAR = 19 ; character code for XOFF

; Zero page allocation:
; $00: X X X X X X X X  X X X X X X X X
; $10: X X X X X X X X  X X X X X X X X
; $20: X X X X X X X _  _ X X X _ _ X X
; $30: X X X X X X X X  X _ _ _ _ _ _ _
; $40: _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _
; $50: _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _
; $60: _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _
; $70: _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _
;
; $80: X X X X X X X X  X X _ _ _ _ _ _
; $90: X X X X X X X X  X X _ _ _ _ _ _
; $a0: _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _
; $b0: _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _
; $c0: _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _
; $d0: _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _
; $e0: _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _
; $f0: _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _

; Non-zero-page memory allocation:
; $000-$0ff -- zero page, see above
; $100-$1ff -- stack
; $200-$21f -- numbufl
; $220-$23f -- numbufh
; $240-$272 -- linctl_a (576) - note, this address is also coded into reload.tcl
; $273-$2a5 -- linctl_b (627)
; $2a6-$2af -- available RAM
; $2b0-$2ff -- tabstops
; $300-$37f -- inbufh
; $380-$3ff -- inbufl

; Misc addresses
irqtmp = $00 ; 2 byte - a pointer that can only be used in interrupt mode
vitmp1 = $0e ; a byte for temporary use in video IRQ
vitmp2 = $0f ; another byte for temporary use in video IRQ
scanline = $02 ; 2 bytes - scan line counter
IF ENABLE_DEVEL
scanlinetmp = $04 ; 2 bytes - temporary copy of scanline
ENDC ; ENABLE_DEVEL
addr0 = $06 ; 2 bytes - pointer to data
atmp = $14 ; 2 bytes - temporary manipulation of addresses, or other
           ; 16-bit integers, when not in interrupt mode
btmp = $1c ; 2 bytes - more temporary 16 bits of storage
prmodesr = $23 ; 1 byte - operator character (set/reset) in prmode()
scanctr = $29 ; 1 byte - times new video frame was generated
IF ENABLE_PROGDELAY
progdelay = $33 ; 2 byte - programmable delay on input events
ENDC
IF ENABLE_VISBELL | ENABLE_AUDBELL
frame4bell = $35 ; 1 byte - if 0, frame has been drawn, used to time the bell
bellon = $36 ; 1 byte - time bell will be on, counted in video frames
ENDC
IF ENABLE_BUFHWm
bufhwm = $37 ; 1 byte - input buffer "high water mark"
ENDC
tfktmp = $25 ; 1 byte - txchar_fromkbd() to preserve accumulator while echoing

; State for "ESC [" escape sequences, including numbuf*, a sequence of 16-bit
; unsigned integers passed as part of the sequence.
NUMBUFSZ = 32 ; how many
numbufl = $200 ; low half of each
numbufh = $220 ; high half of each
numbufn = $09 ; pointer to current entry in numbufl / numbufh
esc_lbvar = $1b ; variant of "ESC [ ..." sequence, one of the following
ESC_LBVAR_NORMAL = 0 ; a normal sequence, no error, no private prefix
                     ; ESC_LBVAR_NORMAL must be zero, some places that
                     ; fact is implicitly used
ESC_LBVAR_QUES = 1 ; DEC private prefix '?'
ESC_LBVAR_EQUAL = 2 ; my private prefix '='
ESC_LBVAR_INVALID = 255 ; something is wrong with sequence, don't perform it

; Basic screen and mode information
DEFFG = 7 ; default foreground color, 7 = WHITE
fontsz = $0a ; how many lines our font has (10, 12, 16)
fontszm1 = $24 ; one less than 'fontsz' (9, 11, 15)
textsz = $0b ; how many text rows our screen has (480 / fontsz)
textszm1 = $31 ; textsz minus 1

; Line control data: The pr*() routines manipulate this to do scrolling
; and the like, and the video_irq code reads it to figure out what to
; actually display.  Contains two arrays of 51 bytes.
; Array 'linctl_a' - address of each row in text RAM
; Array 'linctl_b' - various row attributes:
;       &$0f -- not used (once held: row height, which is inadequate)
;       &$10 -- flag: double width characters
;       &$20 -- not used (once held: bottom row underline)
;       &$40 -- flag: if doing double height chars, this is lower half
;       &$80 -- flag: doing double height chars
linctl_a = $240
linctl_b = $273
linctl_cur = $0c ; number of currently rasterizing entry in linctl_{a,b}
linctl_cursl = $0d ; scan line within that entry
MAX_STORED_ROW = 50 ; highest numbered row that fits in TEXTRAM

; Scroll & cursor control data.  Used by the pr*() functions.
; Before moving the cursor, call cursor_hide().  Most things that move it
; need to clear cursor_defm() too; except in prtab() and prglyph().
; And follow with a call to cursor_figure() to fill in cursor_ptr.
scroll_top = $10 ; top row of scrolling region (0-49)
scroll_bot = $11 ; bottom row of scrolling region (scroll_top-49)
    ; Note: cursor_row & cursor_col are part of the saveable_data block
    ; and shouldn't be reallocated lightly.
cursor_row = $80 ; row the cursor is on (0-49)
cursor_col = $81 ; column the cursor is on (0-79)
cursor_ptr = $19 ; 2 bytes: pointer to the character cell in text RAM
cursor_defm = $2e ; flag: deferred movement at right edge of screen
cursor_shown = $2a ; boolean indicating cursor is visible at the moment
cursor_backing = $2b ; attribute byte under the cursor if it's shown

; Character set selection
; For cset, cset_g0, cset_g1:
;       US - 0, UK - 1, graphic - 64, alternate - 128, 192
; Note: cset* are part of the saveable_data block
; and shouldn't be reallocated lightly.
; Also note, cset_g0 & cset_g1 must be adjacent addresses in that order.
cset = $82 ; current character set
cset_g0 = $83 ; current G0 character set
cset_g1 = $84 ; current G1 character set
cset_sel = $85 ; 1 if current character set is G1, 0 if it's G0

; Other state belonging to the pr*() functions.
prstate = $16 ; state processing control codes, which should be an even number
; Note: prchinv & prchattr* are part of the saveable_data block
; and shouldn't be reallocated lightly.
; Also note, prchattru & prchattri must be adjacent addresses in that order.
prchinv = $86 ; flag: is prchattr inverse video (1 = yes, 0 = no)
prchattr = $87 ; attribute byte for the next glyph we draw
prchattru = $88 ; never inverted version of prchattr
prchattri = $89 ; always inverted version of prchattr
tabstops = $2b0 ; 80 bytes, one per possible tab stop position
saveable_data = $80 ; 10 bytes, various values named separately
restorable_data = $90 ; 10 bytes, saved counterpart of saveable_data

; Input buffer operations; the buffer holds both serial and keyboard inputs.
INBUFSZ = 128 ; number of entries in input buffer; must be power of 2
INBUF_ET_SERCH = 0 ; seen in inbufh for bytes received from serial port
INBUF_ET_SEREX = 1 ; seen in inbufh for exceptions from serial port
INBUF_ET_PS2CH = 2 ; seen in inbufh for bytes received from PS/2 port
INBUFTHR_XOFF1 = 32 ; threshold to send XOFF
INBUFTHR_XOFF2 = 112 ; threshold to send second XOFF
INBUFTHR_XON = 16 ; threshold to send XON
inbufh = $300 ; event types in input buffer
inbufl = $380 ; event values in input buffer (key, char, or other codes)
inbufi = $1e ; where to put next event into the input buffer
inbufo = $1f ; where to take next event out of the input buffer
inbufnotempty = $08 ; indicates that inbuf has something in it
; if inbufi == inbufo then the buffer is either empty or full;
; in that case look at 'inbufnotempty'

; XON/XOFF flow control handling
xofflevel = $32 ; 0-3, last INBUFTHR_* defined range we accounted for
                ; by sending the appropriate XON or XOFF characters:
                ; 0 -- events <= INBUFTHR_XON
                ; 1 -- INBUFTHR_XON < events < INBUFTHR_XOFF1
                ; 2 -- INBUFTHR_XOFF1 <= events < INBUFTHR_XOFF2
                ; 3 -- INBUFTHR_XOFF2 <= events
                ; XON sent: when it becomes zero
                ; XOFF sent: when it becomes two or three

; Keyboard handling
keycoding_state = $2f ; bit fields indicating state of keyboard WRT
                      ; multi-byte scan codes:
                      ;     &$01 - received $E0 (extra keycodes)
                      ;     &$80 - received $F0 (break not make)
keymod_state = $30 ; bit fields indicating state of modifier keys:
KEYMOD_STATE_SHIFT       = $01
KEYMOD_STATE_ALT         = $02
KEYMOD_STATE_SCROLL_LOCK = $04
KEYMOD_STATE_NUM_LOCK    = $08
KEYMOD_STATE_CAPS_LOCK   = $40 ; can be tested for with BIT: goes to the V flag
KEYMOD_STATE_CONTROL     = $80 ; can be tested for with BIT: goes to the N flag

keyled_state = $18 ; keyboard LED state bits as used by the PS/2 keyboard:
                   ;    &$01 - scroll lock
                   ;    &$02 - num lock
                   ;    &$04 - caps lock
keyboard_present = $26 ; We've received an 'ack' from the keyboard & can
                       ; safely send to it.  If the keyboard isn't hooked
                       ; up and we try to send bytes to it, this software
                       ; will hang!

; Mode flag bits, altered via ESC [ ... {l,h} or other ways
modeflags1 = $22
MODEFLAGS1_CHECKERBOARD = $01 ; ESC [ = 1 h -- show checkerboard glyph
                              ;     in some error cases (default: yes)
MODEFLAGS1_BLOCKCURSOR  = $02 ; ESC [ = 3 h -- show cursor as blinking
                              ;     block instead of blinking underline
IF ENABLE_DEVEL
MODEFLAGS1_KEYCODES     = $04 ; ESC [ 161 | -- transmit each byte received
                              ;     from keyboard, on serial, as hex
ENDC ; ENABLE_DEVEL
MODEFLAGS1_ORIGIN       = $08 ; ESC [ ? 6 h -- origin mode, determines
                              ;     how row numbers are handled
MODEFLAGS1_ORIGIN_SAVED = $10 ; must be 2*MODEFLAGS1_ORIGIN; saved copy
                              ; of it in the ^[7 / ^[8 escape sequences
MODEFLAGS1_VISBELL      = $20 ; ESC [ = 4 h -- visible bell (default: yes)
MODEFLAGS1_AUDBELL      = $40 ; ESC [ = 5 h -- audible bell (default: yes)
MODEFLAGS1_CURSORKEY    = $80 ; ESC [ ? 1 h -- cursor key mode (default: no)
modeflags2 = $38
MODEFLAGS2_LFNL         = $01 ; ESC [ 20 h -- linefeed / newline mode (def: no)
MODEFLAGS2_XONXOFF      = $02 ; Generate/honor XON/XOFF control chars
    MODEFLAGS2_XONXOFF_LOG    = 1 ; log base 2 of MODEFLAGS2_XONXOFF
MODEFLAGS2_CLEAR8       = $04 ; Clear the top bit of received bytes
    MODEFLAGS2_CLEAR8_LOG     = 2 ; log base 2 of MODEFLAGS2_CLEAR8
MODEFLAGS2_LOCKKB       = $08 ; ESC [ 2 h -- lock keyboard (def: no)
MODEFLAGS2_WRAP         = $10 ; ESC [ ? 7 h -- auto wrap mode (def: yes)
MODEFLAGS2_NOECHO       = $20 ; ESC [ ? 12 h -- no local echo (def: yes)
    MODEFLAGS2_NOECHO_LOG     = 5 ; log base 2 of MODEFLAGS2_NOECHO
MODEFLAGS2_INSERT       = $40 ; ESC [ 4 h -- char insertion mode (def: no)

; State in menu mode
menu_fld_ptr = $12 ; 2 bytes: ptr to currently selected field in menu_data,
                   ; in particular the $03 byte starting it
menu_saw_lf = $17 ; 1 byte: set by menu_key_next_field() when it
                  ; crosses a line-feed character

;; ;; ;; ;; Code: general utilities

    ; atmp_indirector: Just does 'jmp (atmp)'.  The only purpose for
    ; having it is because there's no 'jsr (atmp)' in the instruction set.
atmp_indirector = *
    jmp (atmp)

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

panic = *
    ; panic(): simple function for system crash.  Sets LEDs and prints a
    ; character on the output serial port, repeatedly, indefinitely.
    sei ; don't be interrupted
    sta LEDOUT ; put it on LEDs
    sta SER_BASE+SER_TX ; put it in output FIFO even if FIFO is full
    jmp panic

IF ENABLE_DEVEL
    ; txdbg(): Transmit the following on the serial port, for debugging:
    ;   the parameter (a character)
    ;   the accumulator value in hex
    ; Note (YYY): Normally the code shouldn't have any uses of this.
txdbg MACRO
    sta $ff
    pha
    lda #\1
    jsr txchar
    tya
    pha
    txa
    pha
    lda $ff
    jsr txhex
    pla
    tax
    pla
    tay
    pla
ENDM
ENDC ; ENABLE_DEVEL

    ; pgalign(): Align whatever followed to a page (256 byte) boundary
    ; in memory.
pgalign MACRO
IF ENABLE_PGALIGN
    * = (* + 255) & 65280
ENDC
ENDM

    ; adj_param_1_127(): Look at the first parameter in numbuf and return
    ; it in the accumulator, adjusted as follows:
    ;       >=128 -> 127
    ;       0 -> 1
    ; Returns with the new value in the accumulator register.
    ; YYY there may be more places that can use this
adj_param_1_127 = *
    lda numbufh
    bne adj_param_1_127__high ; >= 256
    lda numbufl
    bmi adj_param_1_127__high ; 128-255
    bne adj_param_1_127__normal ; 1-127
    ; zero
    lda #1
adj_param_1_127__normal = *
    rts
adj_param_1_127__high = *
    lda #127
    rts

    ; byte2decimal(): Convert to decimal.  Takes a single byte value in
    ; the range 0-99, in the accumulator, and provides two values 0-9 in
    ; A (high digit) and X (low digit).  Also sets the Z flag based on
    ; the value of the high digit.
    ; This implementation is rather lame, there are probably better ones
    ; out there.  It counts down by tens and ones.
    ; Uses all registers.
byte2decimal = *
    ldy #0 ; high digit
    ldx #0 ; low digit
    sec
    ; for the high digit, count down / up by tens
byte2decimal__tens = *
    cmp #10 ; see if we still can
    bmi byte2decimal__ones
    iny ; up by ten
    sbc #10 ; down by ten; this should always leave carry set
    bpl byte2decimal__tens ; should always branch
    ; for the low digit, count down / up by ones
byte2decimal__ones = *
    cmp #1 ; see if we still can
    bmi byte2decimal__done
    inx ; up by one
    sbc #1 ; down by one; this should always leave carry set
    bpl byte2decimal__ones ; this should always branch
byte2decimal__done = *
    tya ; high digit goes in accumulator now; this also sets the Z flag
    rts ; done

;; ;; ;; ;; Code: usermode screen handling utilities

    ; blankchars():
    ; Erase some characters on the screen.  Replaces their character codes
    ; with space, and their character attributes with the current foreground
    ; and background colors (uninverted).
    ; Before calling:
    ;       + call cursor_hide
    ;       + point atmp to the leftmost character cell to be blanked
    ;       + set accumulator to 2x the number of character cells
    ; Uses the accumulator and the Y register; preserves the X register.
blankchars = *
    tay ; put character cell count x2 in Y register to use it
    beq blankchars__done ; if it's zero, there's nothing to do
    pha ; save character cell count x2 for later
    ; first loop: blank the attributes
    lda prchattru ; get uninverted attributes
    and #$77 ; and get the fg & bg color parts of those
blankchars__aloop = *
    dey
    sta (atmp),y ; store attributes
    dey
    bne blankchars__aloop ; do more until all characters are done
    ; second loop: blank the characters
    pla ; retrieve character cell count x2
    tay ; and put it in Y register to use it
    lda #32 ; space character
blankchars__cloop = *
    dey
    dey
    sta (atmp),y ; replace the character code with space
    bne blankchars__cloop ; repeat if there are more characters to do
blankchars__done = *
    rts
    
    ; blankrow50():
    ; Blank the 'MAX_STORED_ROW'th row, which isn't usually visible
    ; but commonly becomes so during scrolling.  Uses atmp for
    ; temporary storage.  Uses all registers, and atmp.
    ; Doesn't blank the row attributes, the caller should do that.
    ; Before calling this, call cursor_hide.
blankrow50 = *
    ldx #MAX_STORED_ROW ; the row we're going to work on
    jsr find_row ; store its pointer in 'atmp'
    lda #SCREEN_WIDTH*2 ; amount to blank
    jmp blankchars ; blankchars() does the blanking, then returns

    ; cursor_figure: compute cursor_ptr based on cursor_row & cursor_col
    ; and the contents of linctl_a
cursor_figure = *
    ldx cursor_row ; visible screen row
    lda linctl_a,x ; map to address in text RAM (32 byte units)
    lsr a
    lsr a
    lsr a ; now it's in 256 byte units
    clc
    adc #TEXTRAM>>8
    sta cursor_ptr+1 ; row start address, high byte
    lda linctl_a,x ; again map to address in text RAM (32 byte units)
    asl a
    asl a
    asl a
    asl a
    asl a ; now it's in byte units
    ; note that TEXTRAM is divisible by 256
    sta cursor_ptr ; row start address, low byte
    lda cursor_col ; cursor position in characters
    asl a ; convert to bytes
    clc
    adc cursor_ptr ; add it to the position
    sta cursor_ptr
    lda cursor_ptr+1
    adc #0
    sta cursor_ptr+1
    rts ; done

    ; invert_screen: turn on full-screen inverse video with "ESC [ ? 5 h"
invert_screen = *
    lda #V_CTL_SETF|V_CTL_FINV
    sta V_CTL
    rts

    ; uninvert_screen: turn off full-screen inverse video with "ESC [ ? 5 l"
uninvert_screen = *
    lda #V_CTL_SETF
    sta V_CTL
    rts

    ; find_row() - Given a row number on the screen (0-MAX_STORED_ROW)
    ; in the X register, find the starting address of that row within
    ; TEXTRAM and store it in 'atmp'.
    ; Preserves the X and Y registers, modifies the A register.
find_row = *
    lda linctl_a,x ; address in 32 byte units within TEXTRAM
    lsr a
    lsr a
    lsr a ; now it's in 256 byte units
    clc
    adc #TEXTRAM>>8
    sta atmp+1 ; row start address, high byte
    lda linctl_a,x ; address in 32 byte units within TEXTRAM
    asl a
    asl a
    asl a
    asl a
    asl a ; now it's in byte units
    ; TEXTRAM is divisible by 256; this code takes advantage of that fact
    sta atmp ; row start address, low byte
    rts ; and it's been computed

    ; cursor_show() - Display the cursor as an underline or block
    ; as selected.
cursor_show = *
    lda cursor_shown ; is it already shown?
    bne cursor_show__ret ; do nothing if it is
cursor_show_raw = *
    inc cursor_shown ; indicate that it's shown
    ldy #1 ; point to attribute byte under it
    lda (cursor_ptr),y ; get current value
    sta cursor_backing ; and save it for later when we hide the cursor
    lda modeflags1 ; see if cursor is block or underline
    and #MODEFLAGS1_BLOCKCURSOR
    beq cursor_show__underline
    ; show cursor as block (inverse video)
    lda cursor_backing ; get what's there now
    cmp #$80 ; two-instruction rotation from http://6502org.wikidot.com/software-8-bit-rotations
    rol a
    cmp #$80
    rol a
    cmp #$80
    rol a
    cmp #$80
    rol a
    and #$77 ; after rotation, etract the bit fields
    sta atmp ; temporarily stash them
    lda cursor_backing ; and get the bits we aren't going to move
    and #$88
    ora atmp ; and combine them
    sta (cursor_ptr),y ; store the result
cursor_show__ret = *
    rts ; done
cursor_show__underline = *
    ; show cursor as underline
    lda cursor_backing ; get what's there now
    eor #$80 ; underline it, or un-underline it
    sta (cursor_ptr),y ; store the result
    rts ; done

    ; cursor_hide() - Hide the cursor if displayed as an underline or
    ; block.  Call this if you modify cursor_col, cursor_row, cursor_ptr,
    ; or TEXTRAM.
cursor_hide = *
    lda #0 ; delay cursor blinking for a time
    sta scanctr
    lda cursor_shown ; is it shown now?
    beq cursor_hide__ret ; if not, do nothing
cursor_hide_raw = *
    ldy #1 ; offset of attributes byte
    lda cursor_backing ; value that should be there without cursor
    sta (cursor_ptr),y ; store it there
    lda #0
    sta cursor_shown ; and it's no longer shown
cursor_hide__ret = *
    rts ; done

    ; cursor_blink() - Show the cursor if it was hidden, hide it if
    ; it was shown.
cursor_blink = *
    lda cursor_shown ; is it shown now?
    bne cursor_blink__hide
    ; show it
    jmp cursor_show_raw
cursor_blink__hide = *
    ; hide it
    jmp cursor_hide_raw

    ; scroll_shift_up(): scroll down one line, that is, shift the scroll area
    ; up one line.  If you want to operate on a different range of rows
    ; than the scroll area, temporarily change scroll_top & scroll_bot.
    ; Uses all registers, and atmp.
scroll_shift_up = *
    ; Initialize the new row that's going to be scrolled in.
    jsr blankrow50
    ; Retrieve the row that's going to be scrolled out.
    ; Stash its linctl_a value for later use.
    ; We don't need to stash the linctl_b values, they'll be clobbered when
    ; we re-use the row.
    ldx scroll_top
    lda linctl_a,x
    pha ; and stash the 1-byte text memory pointer (linctl_a) for later use
    ; Shift the middle rows up by one.
scroll_shift_up__loop = *
    txa ; going to see if we're done with that
    cmp scroll_bot
    beq scroll_shift_up__shifted ; branches when done
    lda linctl_a+1,x ; copy from lower...
    sta linctl_a,x ; ...to higher
    lda linctl_b+1,x ; and likewise for row attributes
    sta linctl_b,x
    inx
    jmp scroll_shift_up__loop ; and repeat
scroll_shift_up__shifted = *
    ; Put the new row into the bottom row.
    lda linctl_a+MAX_STORED_ROW
    sta linctl_a,x
    lda #0
    sta linctl_b,x
    ; Now, replace the hidden 'row 50' with the one we shifted out
    pla ; retrieve the one byte text-memory pointer (linctl_a)
    sta linctl_a+MAX_STORED_ROW
    rts

    ; scroll_shift_down(): scroll up one line, that is, shift the scroll area
    ; down one line.  If you want to operate on a different range of rows
    ; than the scroll area, temporarily change scroll_top & scroll_bot.
    ; Uses all registers, and atmp.
scroll_shift_down = *
    ; Initialize the new row that's going to be scrolled in.
    jsr blankrow50
    ; Retrieve the row that's going to be scrolled out.
    ; Stash its linctl_a value for later use.
    ; We don't need to stash the linctl_b values, they'll be clobbered when
    ; we re-use the row.
    ldx scroll_bot
    lda linctl_a,x
    pha ; and stash the 1-byte text memory pointer (linctl_a) for later use
    ; Shift the middle rows down by one.
scroll_shift_down__loop = *
    txa ; going to see if we're done with that
    cmp scroll_top
    beq scroll_shift_down__shifted ; branches when done
    lda linctl_a-1,x ; copy from higher...
    sta linctl_a,x ; ...to lower
    lda linctl_b-1,x ; and likewise for row attributes
    sta linctl_b,x
    dex
    jmp scroll_shift_down__loop ; and repeat
scroll_shift_down__shifted = *
    ; Put the new row into the top row.
    lda linctl_a+MAX_STORED_ROW
    sta linctl_a,x
    lda #0
    sta linctl_b,x
    ; Now, replace the hidden 'row 50' with the one we shifted out
    pla ; retrieve the one byte text-memory pointer (linctl_a)
    sta linctl_a+MAX_STORED_ROW
    rts

    ; save_cursor() - save various cursor related state variables,
    ; found in saveable_data, to restorable_data; see the ^[7 "DECSC"
    ; escape sequence.
save_cursor = *
    ldx #9 ; number of bytes to copy, minus 1
save_cursor__loop = *
    lda saveable_data,x ; copy a byte
    sta restorable_data,x
    dex ; count it
    bpl save_cursor__loop ; repeat while there are any more
    ; also save one bit of modeflags1, MODEFLAGS1_ORIGIN, as
    ; MODEFLAGS1_ORIGIN_SAVED, which is 2*MODEFLAGS1_ORIGIN.
    lda modeflags1
    and #255-MODEFLAGS1_ORIGIN_SAVED ; remove the 'saved' bit
    sta modeflags1 ; store that
    and #MODEFLAGS1_ORIGIN ; and get the current bit
    asl a ; move it to MODEFLAGS1_ORIGIN_SAVED
    ora modeflags1 ; and put it there
    sta modeflags1
    rts 

    ; restore_cursor() - restore various cursor related state variables,
    ; found in saveable_data, from restorable_data; see the ^[8 "DECRC"
    ; escape sequence.
restore_cursor = *
    jsr cursor_hide ; hide the cursor so we can move it safely
    lda #0
    sta cursor_defm ; cancel a deferred movement, not needed now
    ldx #9 ; number of bytes to copy, minus 1
restore_cursor__loop = *
    lda restorable_data,x ; copy a byte
    sta saveable_data,x
    dex ; count it
    bpl restore_cursor__loop ; repeat while there are any more
    ; also restore one bit of modeflags1, MODEFLAGS1_ORIGIN, from
    ; MODEFLAGS1_ORIGIN_SAVED, which is 2*MODEFLAGS1_ORIGIN.
    lda modeflags1
    and #255-MODEFLAGS1_ORIGIN ; remove the target bit
    sta modeflags1 ; store that
    and #MODEFLAGS1_ORIGIN_SAVED ; and get the saved bit
    lsr a ; move it to MODEFLAGS1_ORIGIN_SAVED
    ora modeflags1 ; and put it there
    sta modeflags1
    jmp cursor_figure ; recompute cursor_ptr & return

;; ;; ;; ;; Code: supervisor mode screen handling

video_irq = *
    ; Handler for the video IRQ, which gets called when it's time to
    ; set up another row's information in V_SC0 & V_SC1.  Uses
    ; linctl_* as its source of information.

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
    ;   &$0f - vertical position within font
    ;   &$10 - double width if enabled
    ;   &$20 - underline if this is a row where underline might appear
    ;   &$40 - one, enable output
    ;   &$80 - one, not in sync pulse
    ; V_SC1 gets: Address of row data within TEXTRAM
    ; Both values are derived from linctl_*
    ldx linctl_cur ; index of current row entry in linctl_{a,b}
    lda linctl_a,x ; address of row data
    sta V_SC1
    lda linctl_b,x ; row attributes information
    sta vitmp1 ; stash it where we can easily get at it
    and #$10 ; the double-width bit just copies as is
    sta vitmp2 ; building up a V_SC0 value in vitmp2
    ; now deal with vertical position within font, taking into account
    ; the possibility of double-height characters
    lda vitmp1 ; check for double height
    rol a ; into flags: C = double height, N = lower half
    bcc video_single_height
    bmi video_lower_half
    ; upper half of a double height character
    lda linctl_cursl
    lsr a
    jmp video_figured_height
video_lower_half = *
    ; lower half of a double height character
    lda linctl_cursl
    clc
    adc fontsz
    lsr a
    jmp video_figured_height
video_single_height = *
    ; single height character
    lda linctl_cursl
video_figured_height = *
    ; At this point, the accumulator contains the row within the font and
    ; vitmp2 contains the rest of what we'll put in V_SC0, except for
    ; the $40 and $80 which are both 1 in any visible row, and $20 which
    ; is underline and depends on the visible row.
    cmp fontszm1 ; is font row == size minus 1?
    bne video_not_underline
    ; ok, this is the underline row, set the underline bit
    ora #$20
video_not_underline = *
    ora #$c0 ; set what're always set
    ora vitmp2 ; combine the two sources of bits
    sta V_SC0 ; and send them to the video controller
    ; Now to advance linctl_cur & linctl_cursl to the next position.
    inc linctl_cursl ; next scan line within the row
    lda linctl_cursl ; figure out if we go to the next row
    cmp fontsz
    bmi video_next ; branch if there are more scan lines in this row
    inc linctl_cur ; new row
    lda #0 ; first scan line of that row
    sta linctl_cursl
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
    lda #0 ; next scan line: zero
    sta scanline
    sta scanline+1
    sta linctl_cur ; and that'll be the first text row
    sta linctl_cursl ; and the first scan line of it
    inc scanctr ; one more frame drawn; used for timing
IF ENABLE_VISBELL | ENABLE_AUDBELL
    sta frame4bell ; one more frame drawn; used for timing
ENDC
    jmp irq ; handle any more IRQs that are pending

    ; video_start(): Startup-time initialization of stuff related to the
    ; screen.  This also initializes some non-video stuff (serial port
    ; interrupts). Run with interrupts disabled.
video_start = *
    ; initialize state used by our video IRQ handler
    lda #0
    sta scanline ; start at scan line zero
    sta scanline+1
    sta linctl_cur ; start with first text row
    sta linctl_cursl ; and first scan line of that
    sta scanctr ; zero for consistent initial timing

    ; initialize text RAM to default foreground on black, spaces
    lda #TEXTRAM&255 ; address
    sta irqtmp
    lda #TEXTRAM>>8
    sta irqtmp+1
    ldx #32 ; number of 256 byte pages to fill in
    ldy #0 ; byte index
video_start__tloop = *
    lda #' ' ; character to put in, space
    sta (irqtmp),y ; put space in that byte
    iny ; next byte
    lda #DEFFG ; attributes to put in, foreground on black
    sta (irqtmp),y ; color that byte
    iny ; next byte
    bne video_start__tloop ; branch if there's more in this page
    ; move on to next page
    inc irqtmp+1
    dex
    bne video_start__tloop ; branch if there're more pages

    ; Look in font RAM to find out how many lines our font has.  We
    ; recognize 8x10, 8x12, and 8x16 fonts.
    lda #FONTRAM>>8 ; figure out address of font info byte
    sta irqtmp+1
    lda #FONT_INFO_CHAR
    sta irqtmp
    ldy #0 ; read it
    lda (irqtmp),y
    and #$1f ; extract font height in pixels
    cmp #10 ; is it an 8x10 font?
    bne video_start__font__not10
    ; 8x10 font = 80x48 text
    lda #10
    sta fontsz
    lda #48
    sta textsz
    bne video_start__font__done ; branches always
video_start__font__not10 = *
    cmp #12
    bne video_start__font__not12
    ; 8x12 font = 80x40 text
    lda #12
    sta fontsz
    lda #40
    sta textsz
    bne video_start__font__done ; branches always
video_start__font__not12 = *
    ; 8x16 font = 80x30 text
    lda #16
    sta fontsz
    lda #30
    sta textsz

video_start__font__done = *
    lda textsz ; we want text size minus 1, compute that now
    sta textszm1
    dec textszm1
    lda fontsz ; we also want font size minus 1, compute that now
    sta fontszm1
    dec fontszm1

    ; Initialize our row table, which is manipulated by the pr*() routines
    ; and used by video_irq to figure out what to do on each scan line.
    ; Initial contents: 51 lines (the maximum currently storable in
    ; TEXTRAM and more than we actually display) with successive addresses
    ; in TEXTRAM and each of font height with underline & no other
    ; decorations.
    ldx #MAX_STORED_ROW ; text line counter and address
    lda #250 ; text address corresponding to it
video_start__linctl_a = *
    sta linctl_a,x ; store the address part
    sec
    sbc #5 ; text address, back 80 chars (160 bytes)
    dex ; previous text line
    bpl video_start__linctl_a ; do more
    ldx #MAX_STORED_ROW ; counter+address again
    lda #$00 ; default line attributes
video_start__linctl_b = *
    sta linctl_b, x ; store the line attributes part
    dex ; previous text line
    bpl video_start__linctl_b ; do more

    ; initialize interrupt handler(s)
    lda #2 ; enabling slot 1's alpha interrupt (video row)
    sta ICTL_ENA0
    lda #$11 ; enabling slot 8's alpha interrupt (serial RX)
             ; and slot 12's alpha interrupt (PS/2 RX)
    sta ICTL_ENA1
    ; the other interrupt bits will already be zero after reset

    ; initialize scroll & cursor state
    lda #0 ; top of scroll area is top of screen
    sta scroll_top
    sta cursor_row ; cursor is in upper left of screen
    sta cursor_col
    sta cursor_shown ; cursor is not visible
    sta cursor_defm ; no deferred movement yet
    ; sta cursor_backing ; doesn't matter when not shown
    jsr cursor_figure ; figure out cursor_ptr
    lda textszm1 ; bottom of scroll area is bottom of screen
    sta scroll_bot

    ; cancel screenwise inverse video
    lda #V_CTL_SETF
    sta V_CTL
IF ENABLE_VISBELL
    lda #V_CTL_SETF2 ; stop the visbell, if it was on
    sta V_CTL_SETF2
ENDC

    ; back to start()
    rts

;; ;; ;; ;; Code: keyboard handling

    ; keyboard_start(): Called at startup time to initialize the keyboard.
    ; Unlike many other *_start() functions, this is *not* called in
    ; interrupt mode.  It can delay.
keyboard_start = *
    ; Reset keyboard.  This also serves as a test for whether one is
    ; connected.
    lda #0
    sta keycoding_state ; neither $E0 or $F0 has been received
    sta keyboard_present ; in fact, nothing has been received
    sta keymod_state ; no modifier keys are active
    sta keyled_state ; no LEDs are active
    jsr keyboard_reset ; try it
    bcc keyboard_start_done ; keyboard has been reset & started its BAT
                            ; (basic assurance test)
    jsr keyboard_reset ; retry if it timed out
    bcc keyboard_start_done
    ; keyboard timed out twice, maybe it's not connected
    ; (or perhaps a reset is going on already or something)
    jsr prstring
    asc "Keyboard not found, check PS/2 port A.\r\n\0"
keyboard_start_done = *
    rts

    ; keyboard_reset(): Tell the keyboard to reset itself.  Waits up to
    ; one second for acknowledgement.  It's run twice at start time, which
    ; means a 2 second delay if no keyboard is found.  During that time,
    ; all input both from the keyboard and the serial port is ignored.
    ; After that time, the keyboard will be running its basic assurance
    ; test (BAT).
    ; Uses all registers.  Returns with carry set in case of timeout, clear
    ; in case of normal result.  Doesn't wait for the BAT to complete.
keyboard_reset = *
    ; note: Normally when sending something to the keyboard, we'd wait
    ; for the FIFO to clear (by checking the beta interrupt line on slot
    ; 12).  But here we don't; if we can't send, it's possibly just because
    ; the keyboard interface is stuck in which case we *want* a timeout.
    lda #$ff ; keyboard reset command
        ; This will reset various things about the terminal.  Including
        ; scan code set and key repeat; and the keyboard will perform
        ; its Basic Assurance Test (BAT).
    sta PS2A_BASE+PS2_TX ; send it to the keyboard
    ; Now consume input until we get the keyboard's ack, $fa, or one second
    ; has elapsed.  For that, we look at scanctr which should increase by
    ; 60 every second.
    ldx #60 ; sixtieths of a second left to wait
    lda scanctr ; current frame counter; we'll check for changes
keyboard_reset__wloop = *
    ldy inbufnotempty ; any input to read?
    bne keyboard_reset__input ; yes, read it
    cmp scanctr ; compare current & saved scan counter values
    beq keyboard_reset__wloop ; nothing has happened & no time has passed
    ; 1/60 second has elapsed
    lda scanctr ; and new current value to compare against
    dex ; one fewer such period to wait
    bne keyboard_reset__wloop ; branch if haven't timed out yet
    ; a whole second has elapsed without acknowledgement; report timeout
    sec
    rts
keyboard_reset__input = *
    ; input is available on inbuf; maybe it's from the keyboard, and maybe
    ; it's what we wanted.
    jsr getinput ; get the input
    cmp #INBUF_ET_PS2CH
    bne keyboard_reset__wloop ; it wasn't from keyboard
    txa
    cmp #$FA
    bne keyboard_reset__wloop ; it wasn't an ACK
    ; keyboard has acknowledged the reset; we're done
    sta keyboard_present ; indicate keyboard present, note accum = $FA != 0
    clc
    rts

    ; keydecode() - Given a byte received from the keyboard (in the X
    ; register) decode and handle it.  Uses all registers and atmp.
keydecode = *
IF ENABLE_DEVEL
    lda #MODEFLAGS1_KEYCODES ; check for ^[[161| debugging feature
    and modeflags1
    beq keydecode__noshow
    ; Debugging feature enabled, to display keycodes in hex; do it.
    txa ; but preserve the X register which has the keycode
    pha
    lda #'['
    jsr txchar
    pla
    pha
    jsr txhex
    lda #']'
    jsr txchar
    pla
    tax
keydecode__noshow = *
ENDC ; ENABLE_DEVEL
    txa ; get the key code
    cmp #$FA ; keyboard acknowledges something, ignore
    beq keydecode__done
    cmp #$FC ; keyboard reports failed BAT (basic assurance test), handle
    beq keydecode__batfail
    cmp #$AA ; keyboard reports successful BAT, ignore
    beq keydecode__done
    cmp #$F0 ; keyboard indicates break code coming up, indicate
    beq keydecode__brkpfx
    cmp #$E0 ; keyboard indicates extended code coming up, indicate
    beq keydecode__extpfx
    lda keycoding_state ; see about modifier prefixes already received
    and #1
    bne keydecode__ext_j ; branch to handle $E0 prefixed codes
    ; received a keycode that wasn't prefixed by $E0
    ; it can be in the range $00-$83 and is looked up as itself in keycode_tbl
    txa
    bpl keydecode__normok ; branch if in range $00-$7f
    cmp #$84
    bmi keydecode__normok ; branch if in valid keycode range
    jmp keydecode__bad ; if not: bad code from keyboard
keydecode__done = *
    rts
keydecode__ext_j = *
    jmp keydecode__ext
keydecode__brkpfx = *
    ; received $F0 which is a prefix for break codes; record that fact
    lda keycoding_state
    ora #$80
    sta keycoding_state
    rts
keydecode__extpfx = *
    ; received $E0 which is a prefix for some extra scancodes; record that fact
    lda keycoding_state
    ora #$01
    sta keycoding_state
    rts
keydecode__batfail = *
    ; the keyboard's basic assurance test (BAT) failed
    jsr prstring
    db 13, 10, 27
    asc "[mKeyboard failed basic assurance test (BAT)."
    db 13, 10, 0
    rts
keydecode__bad = *
    ; received a bad (unrecognized, unsupported, or invalid) code from the
    ; keyboard.
    lda #0 ; cancel any pending prefix bytes
    sta keycoding_state
    lda #CHECKERBOARD_GLYPH
    jmp prglyph ; print a "bad input" symbol & return
keydecode__normok = *
    lda keycode_tbl,x
    jmp keydecode__got
keydecode__ext = *
    ; Got a keycode prefixed by $E0.  It can be in the range $10-$7f and
    ; is looked up as itself in keycode_tbl
    txa
    bmi keydecode__bad ; branch if in range $80-$ff
    cmp #$10
    bmi keydecode__bad ; branch if in range $00-$0f
    lda keycode_tbl+$80,x ; look up in the table
    ; fall through to keydecode__got
keydecode__got = *
    ; Got a value out of 'keycode_tbl'; now to interpret it.
    tay ; TAY here is short for 'CMP #0'
    beq keydecode__bad ; value $00 means bad keycode
    bmi keydecode__notasc ; value >= $80 not ascii
    ; value in the range $01-$7f in the table - transmit it and return
    ; if it's a "make" code, ignore otherwise
    ldx keycoding_state
    bmi keydecode__clrst ; branch for a "break" code, to ignore
    ldx #0 ; clear the state, since its modifiers have been used
    stx keycoding_state
    ; at this point we have a valid keypress event, with a "sort of like
    ; ASCII" value in the accumulator.  That's for keymodify() to handle.
    jmp keymodify
keydecode__clrst = *
    ; Clear state regarding prefixes like $E0 and $F0; they no longer
    ; apply.  Then return; we're doing nothing further with the key.
    lda #0
    sta keycoding_state
    rts
keydecode__notasc = *
    ; This value from keycode_tbl is not ASCII, it's something else.
    cmp #$88 ; now check what kind of key it is
    bmi keydecode__modns ; non-sticky modifier key
    cmp #$90
    bmi keydecode__modst ; sticky modifier key
    cmp #$94
    bmi keydecode__arrow ; arrow key
    cmp #$A0
    beq keydecode__enter_j ; enter/return key
    cmp #$A1
    beq keydecode__apps_j ; apps key
    cmp #$C0
    bmi keydecode__edit_j ; editing keys like insert, delete
    ; unknown key, should not have gotten it out of our own table
    jmp keydecode__bad
keydecode__apps_j = *
    jmp keydecode__apps
keydecode__edit_j = *
    jmp keydecode__edit
keydecode__enter_j = *
    jmp keydecode__enter
keydecode__modns = *
    ; $80-$87: The key is a non-sticky modifier key.  When pressed, set a bit
    ; in keymod_state; when released, clear that bit.  The bits are identified
    ; in a lookup table, keymod_nonsticky_mods_tbl
    ldx keycoding_state
    bmi keydecode__modns_brk
    ; the key has been pressed (a "make" code); set its bit in keymod_state
    tax
    lda keymod_nonsticky_mods_tbl-$80,x
    ora keymod_state
    sta keymod_state
keydecode__mod_done = *
    ; and clear the state of the PS/2 keyboard protocol
    lda #0
    sta keycoding_state
    rts ; and return
keydecode__modns_brk = *
    ; the key (a non-sticky modifier key) has been released (a "break" code);
    ; clear its bit in keymod_state
    tax
    lda keymod_nonsticky_mods_tbl-$80,x
    eor #$ff
    and keymod_state
    sta keymod_state
    ; and clear the PS/2 protocol state, and return
    jmp keydecode__mod_done
keydecode__modst = *
    ; $88-$8f: The key is a sticky modifier key.  When pressed, toggle its bit
    ; in keymod_state.  The bits are identified in a lookup table,
    ; keymod_sticky_mods_tbl.
    ldx keycoding_state ; see if this is a "make" or "break" code
    bmi keydecode__mod_done ; a "break" code: ignore
    tax ; now to look up the bit and flip the bit
    lda keymod_sticky_mods_tbl-$88,x ; find what bit is associated
    bit keymod_state ; and check its current value
    bne keydecode__modst_off ; branch if it's on & is being turned off
    ; the modifier was off: turn it on
    ora keymod_state
    sta keymod_state
    ; and update the corresponding LED bit
    lda keyled_tbl-$88,x ; look up the LED's bit
    ora keyled_state ; set it
keydecode__modst_fini = *
    sta keyled_state
    jmp keyled_update ; update the LEDs on the keyboard, and return
keydecode__modst_off = *
    ; the modifier was on: turn it off
    eor #$ff
    and keymod_state
    sta keymod_state
    ; and update the corresponding LED bit
    lda keyled_tbl-$88,x ; look up the LED's bit
    eor #$ff
    and keyled_state ; clear it
    jmp keydecode__modst_fini ; update LEDs here & on the keyboard & return
keydecode__arrow = *
    ; $90-$93: Arrow keys for cursor movement.  Output:
    ;       ESC (^[)
    ;       [ (cursor key mode off) or O (cursor key mode on)
    ;       A-D
    ldx keycoding_state ; see if this is a "break" code, if so ignore
    bmi keydecode__clrst
    ldx #0 ; and clear any modifiers
    stx keycoding_state
    tax ; save the key code for later
    lda #$1b ; emit ESC, the first byte
    jsr txchar_fromkbd
    lda modeflags1 ; figure out what the second byte should be
    and #MODEFLAGS1_CURSORKEY
    beq keydecode__arrow_bracket
    lda #'O' ; cursor key mode on: emit letter 'O'
    jsr txchar_fromkbd
    jmp keydecode__arrow_final
keydecode__arrow_bracket = *
    lda #'[' ; cursor key mode off: emit '['
    jsr txchar_fromkbd
keydecode__arrow_final = *
    txa ; figure out letter A-D to identify the arrow key
    sec
    sbc #$90-'A'
    jmp txchar_fromkbd ; emit that character and return
keydecode__enter = *
    ; enter/return key: transmits CR or CR+LF depending on mode
    ldx keycoding_state ; if it's a "break" code ignore
    bmi keydecode__clrst_j
    lda #$0d ; transmit CR
    jsr txchar_fromkbd
    lda modeflags2 ; check for linefeed / newline mode
    and #MODEFLAGS2_LFNL
    beq keydecode__enter_ret
    lda #$0a ; transmit LF in addition to the CR above
    jmp txchar_fromkbd; transmit & return
keydecode__enter_ret = *
    rts ; return
keydecode__apps = *
    ; the 'apps' key: currently only used in the combination alt-apps
IF ENABLE_MENU
    ldx keycoding_state ; see if this is a "break" code, if so ignore
    bmi keydecode__clrst_j
    lda keymod_state ; see if 'alt' is down & shift/ctrl are not
    and #KEYMOD_STATE_SHIFT|KEYMOD_STATE_ALT|KEYMOD_STATE_CONTROL
    cmp #KEYMOD_STATE_ALT
    bne keydecode__bad_j
    jmp menu_mode ; alt-apps: go to menu mode; return when it's done
ELSE
    ; menu mode not supported, so apps key not supported
    jmp keydecode__bad
ENDC
keydecode__edit = *
    ; the edit keys like insert & delete: currently only used in the
    ; combination ctrl-alt-delete.
IF ENABLE_MENU
    ldx keycoding_state ; see if this is a "break" code, if so ignore
    bmi keydecode__clrst_j
    cmp #$B1 ; see if it's 'delete'
    bne keydecode__bad_j
    lda keymod_state ; see if alt & ctrl are down & shift is up
    and #KEYMOD_STATE_SHIFT|KEYMOD_STATE_ALT|KEYMOD_STATE_CONTROL
    cmp #KEYMOD_STATE_ALT|KEYMOD_STATE_CONTROL
    bne keydecode__bad_j
    jmp menu_mode ; ctrl-alt-delete: go to menu mode; return when it's done
ELSE
    ; menu mode not supported, so the edit keys not supported
    jmp keydecode__bad
ENDC
keydecode__bad_j = *
    jmp keydecode__bad
keydecode__clrst_j = *
    jmp keydecode__clrst

    ; keymodify() - Takes an "almost ASCII" code identifying a key that's
    ; been pressed, and applies modifier keys like shift and control
    ; to it.  Then performs whatever effect it has.  Uses all registers
    ; and atmp.
keymodify = *
    ; See if this is one of the printing characters; those are almost
    ; the only ones we apply modifiers to.
    tax ; stash the key value so we can use it later; & check its sign too
    bmi keymodify__done ; branch if key value >= $80
    cmp #$20
    bmi keymodify__done ; branch if key value < $20
    ; now check keymod_state, the state of the keyboard modifiers
    lda #KEYMOD_STATE_SHIFT
    bit keymod_state
    ; now branch based on those flags
    bmi keymodify__ctrl ; branch if KEYMOD_STATE_CONTROL is set
    beq keymodify__noshift ; branch if KEYMOD_STATE_SHIFT is not set
    ; shift key is active, apply the lookup table
    lda keymod_shift_tbl-$20,x ; lookup in the table
    tax
keymodify__noshift = *
    bvc keymodify__done ; branch if KEYMOD_STATE_CAPS_LOCK is not set
    ; caps lock key is active, flip case of any alphabetic character, do
    ; nothing to any others
    txa
    and #$df ; ignore case when checking for an alphabetic character
    cmp #'A'
    bmi keymodify__done
    cmp #'Z'+1
    bpl keymodify__done
    ; it's alphabetical and caps lock is on; flip
    txa
    eor #$20
    tax
keymodify__done = *
    jmp keyhandle
keymodify__ctrl = *
    ; the "control" key is down; other modifiers are ignored
    lda keymod_control_tbl-$20,x ; lookup in the table
    tax
    jmp keyhandle

    ; keyhandle() - The final stage of keydecode()/keymodify()/keyhandle():
    ; It receives an "almost ASCII" value, derived from a key press, and
    ; performs its action.  It receives it in the X register.
keyhandle = *
    txa
    jmp txchar_fromkbd ; put the character corresponding to this key,
                       ; on serial port then return

ps2_a_rx_irq = *
    ; IRQ handler for when a byte can be received on the PS/2 port (A).
    ; Copies the scancode (or other byte received) into 'inbuf'.
    ; note: this IRQ should be disabled when the buffer is full, enabled
    ; when it's not.

    ; get the byte and copy it into the buffer
    ldx inbufi
    lda #INBUF_ET_PS2CH ; indicate what kind of event this is
    sta inbufh,x ; store in the buffer
    lda PS2A_BASE+PS2_RX ; byte or exception code
    sta PS2A_BASE+PS2_RX ; make room for another on the device
    sta inbufl,x ; store in the buffer
    
    ; and indicate the buffer is not empty
    lda #1
    sta inbufnotempty

    ; advance the buffer pointer, with wraparound
    inx ; add one to it
    txa ; into the accumulator to do wraparound
    and #INBUFSZ-1 ; do wraparound
    sta inbufi ; store the result

    ; done handling this IRQ; see about enabling/disabling IRQs that use
    ; 'inbuf' and then about any other IRQs that need handling
    jmp handle_inbuf_irq

    ; keyboard_tx(): Transmit a byte (in the accumulator) to the keyboard.
    ; Modifies the A & Y registers; leaves X alone.
    ; Waits until the device is ready to transmit.
keyboard_tx = *
    tay ; save the value for later
    lda keyboard_present
    beq keyboard_tx__ret ; if keyboard is missing or failing don't try to
                         ; send to it
keyboard_tx__wait = *
    lda ICTL_PEB1 ; get beta IRQ status for slots 8-15
    and #$10 ; in particular slot 12 (PS/2 port A) indicating ready to TX
    beq keyboard_tx__wait
    sty PS2A_BASE+PS2_TX ; transmit the byte
keyboard_tx__ret = *
    rts

    ; keyled_update(): Transmit the new keyboard LED settings to the keyboard.
    ; They're found in keyled_state.
keyled_update = *
    lda #$ED ; command byte
    jsr keyboard_tx ; transmit it
    lda keyled_state ; LED state
    and #$07
    jmp keyboard_tx ; transmit it & return

    ; key_repeat_on() & key_repeat_off(): Turn on/off "typematic" repeat
    ; on all keys.  VT102 had a few keys exempt from that; but while it's
    ; possible to do that with PS/2 keyboards, it's not convenient, and anyway,
    ; the terminal emulators I've seen don't seem to do that.
key_repeat_on = *
    lda #$fa ; command byte
    jmp keyboard_tx ; transmit it & return
key_repeat_off =*
    lda #$f8 ; command byte
    jmp keyboard_tx ; transmit it & return

;; ;; ;; ;; Code: serial port (communication to host computer)

    ; serial_start(): Called in interrupt mode at startup time to initialize
    ; the serial port
serial_start = *
    lda #DEFAULT_BAUDRATE_CODE ; 5 bit code identifying the initial baud
                               ; rate setting.  Found in vtj1-config.asm,
                               ; derived from vtj1-config.txt.  If you
                               ; have the menu, you can use that to change
                               ; the baud rate at runtime.  If you don't,
                               ; you're pretty much stuck at one single 
                               ; baud rate.  In many applications that's
                               ; probably just fine.
    sta SER_BASE+SER_TA
    sta SER_BASE+SER_RA
    lda #TXMODE
    sta SER_BASE+SER_TB
    lda #RXMODE
    sta SER_BASE+SER_RB
    rts

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

    ; txchar_fromkbd: Wrapper around txchar() for when transmitting keys
    ; that originated from the keyboard.  These might be suppressed if
    ; the keyboard is locked.  They might also produce echo.
    ; This needs to avoid modifying any registers.  Which is a pain, but
    ; if it modifies a register, and any calling function doesn't expect
    ; that (as it wouldn't for txchar()) it might not be discovered during
    ; ordinary testing without local echo enabled.
txchar_fromkbd = *
    sta tfktmp ; stash the byte value for later
    lda modeflags2
    and #MODEFLAGS2_LOCKKB
    bne txchar_fromkbd__ret ; if keyboard is locked, skip transmission
    lda tfktmp ; retrieve the byte from where we stashed it
    jsr txchar ; transmit the character
    lda modeflags2
    and #MODEFLAGS2_NOECHO
    bne txchar_fromkbd__ret ; "noecho" flag set, so don't do echo, just return
    txa ; stash the X & Y registers so prbyte() won't mess them up
    pha
    tya
    pha
    lda tfktmp ; get the byte back & display it
    jsr prbyte
    pla ; retrieve the X & Y registers from where we stashed them
    tay
    pla
    tax
    lda tfktmp ; and the accumulator too
    rts ; and return
txchar_fromkbd__ret = *
    rts ; return without doing anything more

IF ENABLE_DEVEL
hextbl = *
    ; table of hex digits
    asc "0123456789abcdef"
ENDC ; ENABLE_DEVEL

IF ENABLE_DEVEL
txnewline = *
    ; txnewline(): transmit carriage return & line feed
    lda #13
    jsr txchar
    lda #10
    jsr txchar
    rts
ENDC ; ENABLE_DEVEL

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

IF ENABLE_DEVEL
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
ENDC ; ENABLE_DEVEL

    ; txdecimal() - given an integer in the range 0-99 in the accumulator,
    ; transmit it in decimal.
txdecimal = *
    jsr byte2decimal ; high digit in A, low digit in X, Z flag set for A
    beq txdecimal__1digit ; branch to skip the high digit
    clc ; convert high digit to ASCII & transmit
    adc #'0'
    jsr txchar
txdecimal__1digit = *
    txa ; convert low digit to ASCII & transmit
    clc
    adc #'0'
    jmp txchar ; transmit & return

serial_rx_irq = *
    ; IRQ handler for when a byte can be received on the serial port; copies
    ; the byte or exception into the input buffer (inbuf)
    ; note: this IRQ should be disabled when the buffer is full, enabled
    ; when it's not

    ; get the byte & copy into the buffer
    ldx inbufi
    lda SER_BASE+SER_ST ; serial status byte
    and #1 ; either 0 for ok or 1 for exception
    sta inbufh,x ; store in the buffer
    lda SER_BASE+SER_RX ; serial byte or exception code
    sta SER_BASE+SER_RX ; make room for another on the serial device
    sta inbufl,x ; store in the buffer

    ; and indicate the buffer is not empty
    lda #1
    sta inbufnotempty

    ; advance the buffer pointer, with wraparound
    inx ; add one to it
    txa ; into the accumulator to do wraparound
    and #INBUFSZ-1 ; do wraparound
    sta inbufi ; store the result
    jmp handle_inbuf_irq

;; ;; ;; ;; Code: input event buffer (inbuf*) handling

    ; inbuf_init(): Initialize 'inbuf'. to empty.  Called at startup time,
    ; with interrupts disabled.
inbuf_init = *
    lda #0
    sta inbufi
    sta inbufo
    sta inbufnotempty
IF ENABLE_BUFHWM
    sta bufhwm
ENDC
    lda #2
    sta xofflevel ; after reset, send XON (act like XOFF had been sent before)
    rts

    ; handle_inbuf_irq: This is the end of multiple IRQ handlers, those which
    ; put things in 'inbuf'.  It figures out if those IRQs need to be
    ; disabled because the buffer has become full.  It follows by returning
    ; to the main IRQ handler to handle any more pending IRQs.
    ; When called, it expects the new value of 'inbufi' in the accumulator.
handle_inbuf_irq = *
    cmp inbufo
    bne handle_inbuf_irq__disdone ; if not full, keep it enabled
    ; disable the IRQs since there's no room in the buffer.  They are:
    ;   slot 8 alpha - serial port RX
    ;   slot 12 alpha - PS/2 port (A) RX
    lda ICTL_ENA1
    and #$ee
    sta ICTL_ENA1
handle_inbuf_irq__disdone = *
IF ENABLE_BUFHWM
    lda inbufi ; calculate how many bytes are in buffer
    sec
    sbc inbufo
    cmp bufhwm ; see if it exceeds the high water mark
    bcc handle_inbuf_irq__bufhwmdone ; BCC = branch if less than (unsigned)
    sta bufhwm ; update the high water mark
handle_inbuf_irq__bufhwmdone = *
ENDC
    jmp irq

getinput = *
    ; getinput(): Wait for something on the input buffer, take it off,
    ; and return it.  That "something" might be a byte received on the
    ; serial port.  The byte value is returned in the X register; the
    ; type is returned in the accumulator.

    ; This routine is also responsible for other things that are time
    ; dependent but not enough so to go in the interrupt handler:
    ; blinking the cursor; timing the bell; programmable delay.

IF ENABLE_PROGDELAY
    ; Programmable delay before waiting for an input event: This slows
    ; down this otherwise fast system, for the sake of debugging.
    lda progdelay ; copy the delay into atmp
    sta atmp
    lda progdelay+1
    sta atmp+1
getinput__progdelay_loop = *
    lda atmp ; see if there is any more delay left to do
    ora atmp+1
    beq getinput__progdelay_done ; nope
    ; decrement 'atmp' to count the times through this loop, as our delay
    lda atmp
    sec
    sbc #1
    sta atmp
    lda atmp+1
    sbc #0
    sta atmp+1
    jmp getinput__progdelay_loop
getinput__progdelay_done = *
ENDC ; ENABLE_PROGDELAY

    ; wait until the input buffer is nonempty, or scanctr has reached
    ; CURSOR_FRAMES indicating it's blink time, or frame4bell is zero
    ; indicating it's time to count down 'bellon'.
    lda inbufnotempty ; see if buffer is nonempty
    bne getinput__haveinput ; act if it's not
    lda scanctr ; check the frame counter
    cmp #CURSOR_FRAMES
    bpl getinput__blink
IF ENABLE_AUDBELL | ENABLE_VISBELL
    lda frame4bell
    beq getinput__bellon
ENDC ; ENABLE_AUDBELL | ENABLE_VISBELL
    jmp getinput ; nothing has happened: repeat until something has

IF ENABLE_AUDBELL | ENABLE_VISBELL
getinput__bellon = *
    inc frame4bell ; clear the indicator, so we'll know the next time
    ; count down the frame-times that the bell will remain active
    ldx bellon ; is it even active?
    beq getinput ; nope; keep waiting for something to happen
    dex ; one less frame-time than before
    stx bellon
    bne getinput ; but there are still more; keep waiting
IF ENABLE_AUDBELL
    lda #SYSSFC_NOBEEP ; stop the beep tone
    sta SYSSFC
ENDC
IF ENABLE_VISBELL
    lda #V_CTL_SETF2 ; clear the 2nd inverse video flag, used for visbell
    sta V_CTL
ENDC
    jmp getinput ; wait for something else to happen
ENDC ; ENABLE_AUDBELL | ENABLE_VISBELL

getinput__blink = *
    ; Time to blink the cursor.
    sei ; disable interrupts so our state won't conflict with video_newframe
    lda scanctr ; subtract from the counter, what we're dealing with
    sec
    sbc #CURSOR_FRAMES
    sta scanctr
    cli ; we can enable interrupts again
    jsr cursor_blink ; blink the cursor
    jmp getinput ; now try again to get input

    ; Input available in buffer
getinput__haveinput = *
    sei ; disable interrupts so our buffer state won't be volatile

    ; the input buffer is not empty: get one entry out of it
    ldy inbufo
    lda inbufh,y ; get event type (into A)
    pha ; stash it for later
    ldx inbufl,y ; get event data byte (into X)

    ; and take that entry out of the input buffer now
    iny ; by incrementing the pointer 'inbufo'
    tya ; with wraparound
    and #INBUFSZ-1
    sta inbufo
    cmp inbufi ; has it become empty?
    bne getinput__removed
    lda #0 ; it became empty; indicate that fact
    sta inbufnotempty
getinput__removed = *
    ; whether it was empty or not, it's no longer full, so enable
    ; the interrupt(s) that fill it
    lda ICTL_ENA1
    ora #$11 ; slot 8 alpha interrupt: serial RX
             ; slot 12 alpha interrupt: PS/2 RX
    sta ICTL_ENA1

    cli ; re-enable interrupts so normal life can go on
    pla ; get the event type back from where we stashed it
    rts

    ; send_xon_xoff() - Send XON/XOFF control characters for flow control,
    ; if enabled, when the buffer size reaches certain thresholds.
send_xon_xoff = *
    ; See if the flow control is even enabled.
    lda modeflags2
    and #MODEFLAGS2_XONXOFF
    beq send_xon_xoff__ret ; skip this stuff if not enabled

    ; Compute how many events are waiting in 'inbuf'.
    sei ; disable interrupts while doing so, to be sure of consistent values.
    lda inbufi ; calculate (inbufi - inbufo): buffer size unless it's full

    sec
    sbc inbufo
    and #INBUFSZ-1 ; deal with wraparound
    bne send_xon_xoff__gotsz ; branch if it's nonzero: it's the count
    ; if inbufi == inbufo, then buffer is either empty or full; check
    lda inbufnotempty
    beq send_xon_xoff__gotsz ; branch if it's empty: count is zero
    lda #INBUFSZ ; the size, if buffer is full
send_xon_xoff__gotsz = *
    cli ; re-enable interrupts so normal life can go on

    ; Now the accumulator holds the number of events in the buffer.
    ; Check it against the various thresholds, and convert to an
    ; 'xofflevel' value (0-3) in X.
    ; Note: This logic will break if INBUFZ > 128.
    ldx #0 ; initial value
    cmp #INBUFTHR_XON+1
    bmi send_xon_xoff__gotlvl ; if events <= INBUFTHR_XON
    inx ; level >= 1 since events > INBUFTHR_XON
    cmp #INBUFTHR_XOFF1
    bmi send_xon_xoff__gotlvl ; if INBUFTHR_XON < events < INBUFTHR_XOFF1
    inx ; level >= 2 since events >= INBUFTHR_XOFF1
    cmp #INBUFTHR_XOFF2
    bmi send_xon_xoff__gotlvl ; if INBUFTHR_XOFF <= events < INBUFTHR_XOFF2
    inx ; level = 3 since events >= INBUFTHR_XOFF2
send_xon_xoff__gotlvl = *
    ; Now X holds the "level" of buffer contents to compare with 'xofflevel'.
    txa ; get it into A where it's most convenient to check
    cmp xofflevel
    beq send_xon_xoff__ret ; if it hasn't changed, do nothing
    bpl send_xon_xoff__up ; if it's increased
    ; level has decreased; if it's decreased to zero we'll send XON
    txa ; TXA is here short for "CMP #0"
    bne send_xon_xoff__ret ; nope; send nothing
    ; level has reached zero; send XON
    lda #XON_CHAR
    jsr txchar
    stx xofflevel ; and record the fact that we've done so
send_xon_xoff__ret = *
    rts
send_xon_xoff__up = *
    ; level has increased; if it's increased to 2 or 3 we'll send XOFF
    cmp #2
    bmi send_xon_xoff__ret ; nope; send nothing
    ; level has reached 2 or 3; send XOFF
    lda #XOFF_CHAR
    jsr txchar
    stx xofflevel ; and record the fact that we've done so
    rts

;; ;; ;; ;; Code: "main stuff": loops, dispatchers, initializers

    ; start: This is the first code to run at startup or after reset.
    ; It sets up I/O devices and memory and then goes to main_loop.
start = *
    ; disable interrupts for now
    sei

    ; and disable decimal mode, which VTJ-1 neveruses
    cld

    ; initialize stack pointer
    ldx #$ff
    txs

    ; blank the LEDs
    lda #0
    sta LEDOUT
IF ENABLE_VISBELL | ENABLE_AUDBELL
    ; lda #0
    sta bellon
ENDC

IF ENABLE_PROGDELAY
    ; and do without the programmable delay
    ; lda #0
    sta progdelay
    sta progdelay+1
ENDC

IF ENABLE_AUDBELL
    lda #SYSSFC_NOBEEP ; stop the bell, if it was on
    sta SYSSFC
ENDC

    ; initialize serial port on slot 8
    jsr serial_start

    ; initialize PS/2 port on slot 12
    ; clear the "exception" bits by writing a 1 to each
    lda #PS2_EXC_RXOFLOW|PS2_EXC_RXPARITY|PS2_EXC_TXBADACK
    sta PS2A_BASE+PS2_EXC

    ; initialize the video display & stuff that goes with it
    jsr video_start

    ; initialize other state regarding the pr*() functions
    lda #PRSTATE_NORMAL
    sta prstate ; starting state: normal characters are, well, normal
    lda #0
    sta prchinv ; starting out: not inverse video
    lda #DEFFG ; starting out: default foreground color on black
    sta prchattr
    sta prchattru
    lda #DEFFG<<4 ; and the inverse of that
    sta prchattri
    lda #$00 ; use US character set for G0 and G1
    sta cset_g0
    sta cset_g1
    sta cset ; and use G0
    sta cset_sel
    jsr initial_tabstops

    ; initialize mode state bits
    lda #MODEFLAGS1_CHECKERBOARD|MODEFLAGS1_VISBELL|MODEFLAGS1_AUDBELL
    sta modeflags1
    lda #MODEFLAGS2_XONXOFF|MODEFLAGS2_WRAP|MODEFLAGS2_NOECHO
    sta modeflags2

    ; empty the input event buffer
    jsr inbuf_init

    ; enable interrupts
    cli

    ; Clear any video errors, which can happen while interrupts are disabled.
    lda #V_CTL_CLEAR
    sta V_CTL

    ; initialize restorable_data by copying from saveable_data
    jsr save_cursor

    ; Keyboard initialization has been saved until after interrupts are
    ; enabled, because it can delay.
    jsr keyboard_start

    ; print a greeting message
    jsr prstring
    asc "VTJ-1 Terminal"
    db 27, 35, 51, 13, 10
    asc "VTJ-1 Terminal"
    db 27, 35, 52, 13, 10
    asc "Software version: "
    include vtj1vers.asm ; that file is autogenerated by vtj1vers.tcl
    db 13, 10, 0

    ; and ready to go!
    jmp main_loop

    ; main_loop: Wait for something to happen; deal with it; and repeat
    ; indefinitely.
main_loop = *

    ; send XON/XOFF for flow control if warranted
    jsr send_xon_xoff

    ; wait for an input byte or an exception or something of interest
    jsr getinput

    ; Now A indicates the event type (INBUF_ET_*); the event detail is in X;
    ; deal with it.
    tay ; TAY here is a substitute for cmp #0
    beq main_loop__gotchar
    cmp #INBUF_ET_SEREX
    beq main_loop__gotexc
    cmp #INBUF_ET_PS2CH
    beq main_loop__gotps2
    ; unknown event code! should never happen
    ; fall through to main_loop__gotexc
main_loop__gotexc = *
    ; INBUF_ET_SEREX - input exception; indicate with glyph 10 from the font
    jsr prcontrol_unk
    jmp main_loop
main_loop__gotchar = *
    ; INBUF_ET_SERCH - character code from serial; print it and repeat
    txa ; retrieve the character code
    bmi main_loop__gotchar_high ; see about clearing the high bit maybe
    jsr prbyte
    jmp main_loop
main_loop__gotchar_high = *
    ; got character from serial, value >= 128; see if we're supposed to
    ; clear that top bit or not
    lda modeflags2 ; look at the mode setting
    and #MODEFLAGS2_CLEAR8
    bne main_loop__gotchar_clear8 ; yes, clear that top bit
    ; nope: print
    txa
    jsr prbyte
    jmp main_loop
main_loop__gotchar_clear8 = *
    ; print after clearing top bit
    txa
    and #127
    jsr prbyte
    jmp main_loop
main_loop__gotps2 = *
    ; INBUF_ET_PS2CH - got a byte from the PS/2 port, perhaps a keyboard
    ; scan code; process it.
    jsr keydecode
    jmp main_loop

    ; irqbrk:  The main interrupt handler; the CPU jumps to this when an
    ; IRQ signal is raised by some I/O device, or when the BRK instruction
    ; is executed.  In VTJ-1, the BRK instruction is not to be executed.
irqbrk = *
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

    ; irq: We got an interrupt request (IRQ).  Figure out which one to handle.
irq = *
    lda ICTL_PRI ; what's the highest priority interrupt?
    bmi irq__done
    asl a ; look up the IRQ number in the dispatch table
    ; YYY maybe rewrite irq_tbl to use the rts trick, http://wiki.nesdev.com/w/index.php/RTS_Trick
    tax
    lda irq_tbl,x
    sta irqtmp
    lda irq_tbl+1,x
    sta irqtmp+1
    jmp (irqtmp)

unimplemented_irq = *
    ; Handler for an IRQ that doesn't have a handler; it shouldn't have been
    ; enabled and it shouldn't have happened.
    lda #'J'
    jmp panic

;; ;; ;; ;; Code: general display of text

    ; prbyte: "print" a byte, that is, display it.  Most of the time most
    ; byte values get displayed as characters, but some are taken as control
    ; codes instead, and prbyte is the entry point for all of that.
    ; The input byte is in the accumulator.  All registers get mangled.
prbyte = *
    ; prbyte() is mainly a dispatch routine, that looks at 'prstate' and
    ; goes to one of the functions in prstate_tbl.
    cmp #0
    bne prbyte__notnul
    rts ; NUL character has no effect
prbyte__notnul = *
    cmp #22
    bne prbyte__notsyn
    rts ; SYN (synchronous idle) character has no effect
prbyte__notsyn = *
    cmp #24 ; CAN (Cancel) character
    beq prbyte__cancel
    cmp #26 ; SUB (Substitute) character
    beq prbyte__cancel
    pha ; save the byte
    ldx prstate ; look up the state, prstate, in prstate_tbl
    lda prstate_tbl,x
    sta atmp
    lda prstate_tbl+1,x
    sta atmp+1
    pla ; retrieve the byte
    jmp (atmp) ; jump to it
prbyte__cancel = *
    ; input characters CAN (24) or SUB (26) which terminate a control
    ; or escape sequence
    jmp prcontrol_unk ; print the unknown-sequence character

    ; prbyte_normal: prbyte() when state is 0, meaning most characters
    ; are treated as "normal", but control characters are still recognized.
prbyte_normal = *
    tax ; just using TAX to set the sign bit based on A
    bmi prbyte_normal__upper128 ; branch for chars 128-255
    cmp #32
    bmi prbyte_normal__control ; branch for control chars
    cmp #127
    beq prbyte_normal__delete ; branch for DEL
    ; Ok, this is a normal printing character.  Display it, but first,
    ; we have to map it according to the current character set.  The
    ; character sets are:
    ;       US ("B") - found in the font under the same character codes
    ;       UK ("A") - almost same as US, but char 35 maps to glyph 30
    ;       graphic ("0") - same as US for 32-94; 95-126 map to 0-31
    ;       alternate ROM ("1") - same as US, but +128 to glyph numbers
    ;       alternate ROM special ("2") - same as graphic, but +128
    ; The one we're currently using is represented in 'cset'
    bit cset
    bvs prbyte_normal__02
    bmi prbyte_normal__12
    ldx cset
    beq prbyte_normal__B
    ; set "A": like "B", but one character is mapped, 35
    cmp #35
    bne prbyte_normal__B ; not 35, so not mapped
    lda #30 ; in UK character set, char 35 maps to glyph 30
prbyte_normal__B = *
    ; Character set "B", or already mapped: Accumulator has glyph code.
    jmp prglyph
prbyte_normal__02 = *
    ; In character sets 0 & 2, map 95-126 to 0-31
    cmp #95
    bmi prbyte_normal__02_mapped ; not mapped
    sec
    sbc #95
prbyte_normal__02_mapped = *
    ; Did the mapping for character sets 0 & 2; for character set 0 that's
    ; all, for character set 2 there's more mapping.
    bit cset
    bmi prbyte_normal__12
    jmp prglyph ; that's all
prbyte_normal__12 = *
    ; In character sets 1 & 2, map 0-127 to 128-255
    ora #128
    jmp prglyph
prbyte_normal__upper128 = *
    ; bytes 128-255
    cmp #155
    beq prbyte_normal__csi8
    ; Otherwise, treat as illegal character.  There *are* other things
    ; to do but they're a bit complicated.  And VT102 did not
    ; recognize receiving characters >= 128.
    jmp prcontrol_unk ; prints a checkerboard, or does nothing
prbyte_normal__csi8 = *
    ; byte 155, 8-bit shorthand for ESC [
    jmp prcsi ; call prcsi() and return
prbyte_normal__delete = *
    ; byte 127, the ASCII "DEL" (delete) code.  Which on the
    ; receiving/displaying end, does nothing
    rts
prbyte_normal__control = *
    ; bytes 0-31, ASCII control characters.  Dispatch via prcontrol_tbl
    pha ; stash the character code
    asl a ; double it
    tax ; will use as index
    lda prcontrol_tbl,x ; to look up control character actions
    sta atmp
    lda prcontrol_tbl+1,x
    sta atmp+1
    pla ; retrieve the character code
    jmp (atmp)

    ; prcontrol_unk: Unknown/unimplemented control character
prcontrol_unk = *
    lda #PRSTATE_NORMAL
    sta prstate ; go to normal state (YYY: not sure it's always right)
    lda modeflags1 ; see if we're to print a checkerboard character
    and #MODEFLAGS1_CHECKERBOARD
    bne prcontrol_unk__checker
    rts ; return without printing the checkerboard character
prcontrol_unk__checker = *
    lda #CHECKERBOARD_GLYPH ; print checkerboard character
    jmp prglyph

    ; prcontrol_esc: ESC character starting an escape sequence
prcontrol_esc = *
    lda #PRSTATE_ESC
    sta prstate ; how characters after the ESC will be handled
    rts

    ; prglyph: display a single character code as found in the font; this
    ; encoding is different from what we take from input and is more raw
prglyph = *
    pha ; save the byte that identifies the glyph to draw
    ; hide the cursor so we can move it safely & draw under it too
    jsr cursor_hide
    ; handle a deferred cursor movement if there is one and if autowrap
    ; is enabled
    lda cursor_defm ; see if there is one
    beq prglyph__did_defm ; branch if not
    lda modeflags2 ; see if autowrap is enabled (it is by default)
    and #MODEFLAGS2_WRAP
    beq prglyph__did_defm ; branch if not
    jsr prnextline ; move the cursor to left edge of next line
                   ; which should also clear cursor_defm for us
prglyph__did_defm = *
    ; see if we're in insert mode, shifting the rest of the row right
    lda modeflags2
    and #MODEFLAGS2_INSERT
    beq prglyph__did_ins ; branch if not
    jsr prinschar ; shift the right side of the line, right one position
prglyph__did_ins = *
    pla
    ; draw the glyph, with current attributes
    ldy #0
    sta (cursor_ptr),y ; and store it into memory
    iny
    lda prchattr ; and get the attributes
    sta (cursor_ptr),y ; and store them too
    ; see if we're at the right edge
    lda cursor_col
    cmp #SCREEN_WIDTH-1
    bpl prglyph__defer_move
    ; not past the right edge, just move right (two bytes ahead)
    inc cursor_col ; one position
    lda cursor_ptr
    clc
    adc #2
    sta cursor_ptr
    lda cursor_ptr+1
    adc #0
    sta cursor_ptr+1
    rts
prglyph__defer_move = *
    ; cursor was at right edge; delay moving it until later
    lda #1
    sta cursor_defm
    rts

    ; prstring: "print" a zero terminated byte string via prbyte().
    ; the byte string follows the jsr to prstring.  Also takes some
    ; special control characters that aren't accepted from the host computer:
    ;       ctrl-A: calls prnarrowrow() which can be used to make
    ;           single width double height rows
prstring = *
    jsr raget ; get a byte after the jsr to prstring
    beq prstring__zero ; if it's zero, end
    cmp #1 ; if it's control-A (1), do prnarrowrow() and continue
    beq prstring__ctrla
    jsr prbyte ; other characters, print via prbyte()
    jmp prstring ; repeat for next character
prstring__zero = *
    ; the byte was zero; we're done
    rts
prstring__ctrla = *
    jsr prnarrowrow
    jmp prstring ; repeat for next character

;; ;; ;; ;; Code: escape-sequence decoding

    ; prbyte_esc: Received a byte after ESC
prbyte_esc = *
    pha ; stash the byte value temporarily
    lda #PRSTATE_NORMAL ; back to default state unless something does otherwise
    sta prstate
    pla ; retrieve the byte value temporarily stashed
    cmp #'['
    beq prbyte_esc__lb
    cmp #'#'
    beq prbyte_esc__ps
    cmp #'('
    beq prbyte_esc__lpar
    cmp #')'
    beq prbyte_esc__rpar
    cmp #'H'
    beq prbyte_esc__h
    cmp #'c'
    beq prbyte_esc__c
    cmp #'D'
    beq prbyte_esc__dd
    cmp #'M'
    beq prbyte_esc__mm
    cmp #'E'
    beq prbyte_esc__ee
    cmp #'7'
    beq prbyte_esc__7
    cmp #'8'
    beq prbyte_esc__8
    cmp #'Z'
    beq prbyte_esc__Z
    ; unknown sequence: ignored
    rts
prbyte_esc__lb = *
    ; ESC [
    jmp prcsi
prbyte_esc__ps = *
    ; ESC #
    lda #PRSTATE_ESCPS ; change how the next byte is processed
    sta prstate
    rts
prbyte_esc__lpar = *
    ; ESC ( - character set settings
    lda #PRSTATE_ESCLPAR ; change how the next byte is processed
    sta prstate
    rts
prbyte_esc__rpar = *
    ; ESC ) - character set settings
    lda #PRSTATE_ESCRPAR ; change how the next byte is processed
    sta prstate
    rts
prbyte_esc__h = *
    ; ESC H - setting a tab stop
    ldx cursor_col
    lda #1
    sta tabstops,x
    rts
prbyte_esc__c = *
    ; ESC c - reset to initial state (RIS) which I choose to implement
    ; as: restart the software
    jmp start
prbyte_esc__dd = *
    ; ESC D - "IND" escape sequence, move cursor down & maybe scroll
    jmp prindex
prbyte_esc__mm = *
    ; ESC M - "RI" escape sequence, move cursor up & maybe scroll
    jmp prrindex
prbyte_esc__ee = *
    ; ESC E - "NEL" escape sequence, move cursor down & left column &
    ; maybe scroll.
    jmp prnextline
prbyte_esc__7 = *
    ; ESC 7 - "DECSC" escape sequence, save cursor data
    jmp save_cursor
prbyte_esc__8 = *
    ; ESC 8 - "DECRC" escape sequence, restore saved cursor data
    jmp restore_cursor
prbyte_esc__Z = *
    ; ESC Z - "DECID" escape sequence, transmit device information
    jmp txdevatt

    ; prcsi(): Handle the control sequence introduction, either ^[[ (27 91)
    ; or 155.  This is the beginning of many of the ANSI escape sequences.
prcsi = *
    lda #PRSTATE_ESCLB1 ; change how the next byte is processed
    sta prstate
    lda #0
    sta numbufn ; only one number in numbufh & numbufl now
    sta numbufl ; and it's zero
    sta numbufh
    ; implied: lda #ESC_LBVAR_NORMAL (which is zero)
    sta esc_lbvar ; and the sequence is plain, so far
    rts

prbyte_esclbi = *
    ; We've been processing an "ESC [ ... " code, that contains at least
    ; one intermediate byte.  We don't implement any intermediate bytes,
    ; so the sequence is accepted but not implemented.  We continue to
    ; read it up to an invalid or an intermediate byte.
    tax ; TAX is here short for CMP #0
    bmi prbyte_esclb_bad ; a byte that doesn't belong (128-255)
    cmp #127
    beq prbyte_esclb_bad ; a byte that doesn't belong (127)
    cmp #32
    bmi prbyte_esclb_bad ; a byte that doesn't belong (0-31)
    cmp #48
    bmi prbyte_esclbi__nop ; another intermediate byte (32-47), just consume
    cmp #64
    bmi prbyte_esclb_bad ; a byte that doesn't belong here (48-63)
    ; it's a final byte, the sequence is over, ignore the sequence and
    ; get back to normal operation
    lda #PRSTATE_NORMAL
    sta prstate
prbyte_esclbi__nop = *
    rts

    ; prbyte_esclb_* - see prbyte_esclb, of which they are branches
prbyte_esclb_bad = *
    ; In an "ESC [ ..." sequence, a byte that doesn't belong and that's so
    ; messed up we'll just end the sequence now.
    jmp prcontrol_unk
prbyte_esclb_badp = *
    ; In an "ESC [ ..." sequence, a byte that we don't accept but isn't
    ; so bad as to end the sequence, so instead, we just record the
    ; fact that it's bad for later.
    lda #ESC_LBVAR_INVALID
    sta esc_lbvar
    rts
prbyte_esclb_intermed = * ; an intermediate byte (32-47)
    ; In an "ESC [ ..." sequence, a byte in the range 32-47 is an
    ; "intermediate byte" that comes shortly before the final byte.
    ; We don't currently recognize any intermediate bytes, so we
    ; won't do anything on that sequence, but we will read it to
    ; the end.
    lda #PRSTATE_ESCLBI
    sta prstate
    rts
    ; prbyte_esclb1: Received a byte after ESC [ & no other byte
prbyte_esclb1 = *
    cmp #'?' ; is this an ESC [ ? sequence?
    bne prstate_esclb1__notques ; branch if not
    lda #ESC_LBVAR_QUES
    sta esc_lbvar ; record the fact that it's "ESC [ ?"
    lda #PRSTATE_ESCLB ; and that the first character is past
    sta prstate
    rts ; and that's all the '?' is for
prstate_esclb1__notques = *
    cmp #'=' ; is this an ESC [ = sequence?
    bne prbyte_esclb ; branch if not
    lda #ESC_LBVAR_EQUAL
    sta esc_lbvar ; record the fact that it's "ESC [ ="
    lda #PRSTATE_ESCLB ; and that the first character is past
    sta prstate
    rts ; and that's all the '?' is for

    ; prbyte_esclb: Receiving bytes after ESC [
    ; ESC [ is, in ECMA-48, followed by:
    ;       + zero or more bytes in the range 48-63 (parameter bytes)
    ;       + zero or more bytes in the range 32-47 (intermediate bytes)
    ;       + a byte in the range 64-126 (final byte)
    ; This implementation supports the following subset:
    ;       + optional byte '?', which starts a completely different
    ;       set of commands and is handled in prbyte_esclb1, which see
    ;       + zero or more bytes '0' - '9' and ';', taken as 16-bit decimal
    ;       integers with delimiters
    ;       + followed by one byte 64-126, the command byte
    ; Commands either take a limited number of numeric parameters, or are 'm'.
    ; As parameters are parsed, they get stored in two ways, suitable for
    ; both kinds of command.
prbyte_esclb = *
    pha ; save the byte we received
    lda #PRSTATE_ESCLB ; change how the next byte is processed
    sta prstate
    pla ; get back the byte we received
    tax ; TAX is short for CMP #0 here
    bmi prbyte_esclb_bad ; a byte that doesn't belong (128-255)
    cmp #127
    beq prbyte_esclb_bad ; a byte that doesn't belong (127)
    cmp #64
    bpl prbyte_esclb_final ; a final byte (64-126)
    cmp #32
    bmi prbyte_esclb_bad ; a byte that doesn't belong (0-31)
    cmp #48
    bmi prbyte_esclb_intermed ; an intermediate byte (32-47)
    ; a parameter byte (48-63)
    cmp #';' ; check for delimiter
    beq prbyte_esclb_delim
    sec
    sbc #'0' ; hopefully it's a digit; get the digit value
    cmp #10
    bpl prbyte_esclb_badp ; a parameter byte we don't recognize
    ; it's a digit; add it into numbuf*
    pha ; save it for later
    ldx numbufn ; get pointer to current one
    lda numbufl,x ; multiply current by two
    asl a
    sta atmp
    lda numbufh,x
    rol a
    sta atmp+1
    asl atmp ; multiply by two again: that makes four
    rol atmp+1
    clc ; and four plus one will make five
    lda numbufl,x
    adc atmp
    sta atmp
    lda numbufh,x
    adc atmp+1
    sta atmp+1
    asl atmp ; and five times two will be ten
    rol atmp+1
    pla ; now we can add our digit in
    clc
    adc atmp
    sta numbufl,x
    lda atmp+1
    adc #0
    sta numbufh,x
    rts ; and that's all for processing one digit, whew
prbyte_esclb_final = *
    ; This is a final character, which determines what effect all
    ; the parameters that came before will have (*why* does ECMA-48 have
    ; it that way?)
    pha ; preserve the character code
    lda #PRSTATE_NORMAL
    sta prstate
    lda esc_lbvar ; so how did the sequence go?
    ; implied: cmp #ESC_LBVAR_NORMAL (it's zero)
    beq prbyte_esclb_normalseq ; went normally
    cmp #ESC_LBVAR_QUES
    beq prbyte_esclbq ; it was an "ESC [ ? ..." sequence
    cmp #ESC_LBVAR_EQUAL
    beq prbyte_esclbq ; it was an "ESC [ = ..." sequence
    ; it was an invalid "ESC [ ..." sequence
    pla
    jmp prcontrol_unk
prbyte_esclb_delim = *
    lda numbufn ; see if numbuf* is full
    cmp #NUMBUFSZ-1
    bpl prbyte_esclb_delim_zeroit ; it's full: re-use the last entry
    ; it's not full, so move on to a new entry
    inc numbufn
prbyte_esclb_delim_zeroit = *
    ; zero the new entry in numbuf*
    ldx numbufn
    lda #0
    sta numbufl,x
    sta numbufh,x
    rts
prbyte_esclb_normalseq = * ; a normal "ESC [ ..." sequence
    pla ; restore the character code
    cmp #'m' ; is it "set graphical rendition" (character attributes)?
    beq prbyte_esclb__goto_prattcodes
IF ENABLE_DEVEL
    cmp #'|' ; is it VTJ-1 developer controls?
    beq prbyte_esclb__goto_prdevel
ENDC ; ENABLE_DEVEL
    cmp #'r' ; is it set scroll region?
    beq prbyte_esclb__goto_prscrollrgn
    cmp #'h' ; is it a "set mode"?
    beq prbyte_esclb__goto_prmode
    cmp #'l' ; is it a "reset mode"?
    beq prbyte_esclb__goto_prmode
    cmp #'g' ; is it clear tab stops?
    beq prbyte_esclb__goto_prcleartabs
    cmp #'P' ; is it delete characters right of cursor?
    beq prbyte_esclb__goto_prdelchars
    cmp #'J' ; is it clear half or all of screen
    beq prbyte_esclb__goto_prerase
    cmp #'A' ; is it cursor up to margin
    beq prbyte_esclb__goto_prcursorup
    cmp #'B' ; is it cursor down to margin
    beq prbyte_esclb__goto_prcursordown
    cmp #'C' ; is it cursor right to margin
    beq prbyte_esclb__goto_prcursorright
    cmp #'D' ; is it cursor left to margin
    beq prbyte_esclb__goto_prcursorleft
    cmp #'H' ; is it cursor position
    beq prbyte_esclb__goto_prcursorpos
    cmp #'f' ; is it horizontal and vertical position
    beq prbyte_esclb__goto_prcursorpos
    cmp #'K' ; is it erase in line
    beq prbyte_esclb__goto_preraseinline
    cmp #'L' ; is it insert line
    beq prbyte_esclb__goto_prinsertline
    cmp #'M' ; is it delete line
    beq prbyte_esclb__goto_prdeleteline
    cmp #'c' ; is it device attributes
    beq prbyte_esclb__goto_prdevatt
    cmp #'n' ; is it status report
    beq prbyte_esclb__goto_prstatus
    cmp #'q' ; is it update LEDs
    beq prbyte_esclb__goto_prleds
    ; unknown sequence: ignored
    rts
prbyte_esclbq = *
    ; an "ESC [ ? ..." or "ESC [ = ..." sequence
    pla ; restore the character code
    cmp #'h' ; is it a "set mode"?
    beq prbyte_esclb__goto_prmode
    cmp #'l' ; is it a "reset mode"?
    beq prbyte_esclb__goto_prmode
    ; unknown sequence: ignored
    rts
prbyte_esclb__goto_prattcodes = *
    jmp prattcodes ; run prattcodes() and return
prbyte_esclb__goto_prmode = *
    jmp prmode ; run prmode() and return
prbyte_esclb__goto_prcleartabs = *
    jmp prcleartabs ; run prcleartabs() and return
prbyte_esclb__goto_prdelchars = *
    jmp prdelchars ; run prdelchars() and return
prbyte_esclb__goto_prerase = *
    jmp prerase ; run prerase() and return
prbyte_esclb__goto_prcursorup = *
    jmp prcursorup ; run prcursorup() and return
prbyte_esclb__goto_prcursordown = *
    jmp prcursordown ; run prcursordown() and return
prbyte_esclb__goto_prcursorright = *
    jmp prcursorright ; run prcursorright() and return
prbyte_esclb__goto_prcursorleft = *
    jmp prcursorleft ; run prcursorleft() and return
prbyte_esclb__goto_prcursorpos = *
    jmp prcursorpos ; run prcursorpos() and return
prbyte_esclb__goto_preraseinline = *
    jmp preraseinline ; run preraseinline() and return
prbyte_esclb__goto_prinsertline = *
    jmp prinsertline ; run prinsertline() and return
prbyte_esclb__goto_prdeleteline = *
    jmp prdeleteline ; run prdeleteline() and return
IF ENABLE_DEVEL
prbyte_esclb__goto_prdevel = *
    jmp prdevel ; run prdevel() and return
ENDC ; ENABLE_DEVEL
prbyte_esclb__goto_prscrollrgn = *
    jmp prscrollrgn ; run prscrollrgn() and return
prbyte_esclb__goto_prdevatt = *
    jmp prdevatt ; run prdevatt() and return
prbyte_esclb__goto_prstatus = *
    jmp prstatus ; run prstatus() and return
prbyte_esclb__goto_prleds = *
    jmp prleds ; run prleds() and return

prbyte_esclpar = *
    ; handle "ESC (" sequences, which set the G0 character set
    jsr decode_charset ; what character set is selected?
    bcs prbyte_esclpar__ret ; branch if it wasn't recognized
    sta cset_g0 ; store it as the G0 set
    ldx cset_sel ; see which one is selected, G0 or G1
    bne prbyte_esclpar__ret
    sta cset ; if G0 is selected, change it
prbyte_esclpar__ret = *
    lda #PRSTATE_NORMAL ; we've read the control sequence; it's over
    sta prstate
    rts

prbyte_escrpar = *
    ; handle "ESC )" sequences, which set the G1 character set
    jsr decode_charset ; what character set is selected?
    bcs prbyte_esclpar__ret ; branch if it wasn't recognized
    sta cset_g1 ; store it as the G1 set
    ldx cset_sel ; see which one is selected, G0 or G1
    beq prbyte_escrpar__ret
    sta cset ; if G1 is selected, change it
prbyte_escrpar__ret = *
    lda #PRSTATE_NORMAL ; we've read the control sequence; it's over
    sta prstate
    rts

    ; prbyte_escps: Received a byte after ESC #
    ; Dispatch off table prescps_tbl
prbyte_escps = *
    pha ; stash the character value
    sec
    sbc #'0' ; convert to index into prescps_tbl
    bmi prbyte_escps__inval ; isn't in range
    cmp #11
    bpl prbyte_escps__inval ; isn't in range
    asl a ; now look up in the 2-byte-per entry table
    tax
    lda prescps_tbl,x
    sta atmp
    lda prescps_tbl+1,x
    sta atmp+1
    lda #PRSTATE_NORMAL ; and back to normal state unless we go to another one
    sta prstate
    pla ; get back the character value
    jmp (atmp) ; and jump to the handler
prbyte_escps__inval = *
    ; invalid "ESC #" sequence
    pla ; forget what exactly it was, it doesn't matter
    jmp prcontrol_unk

;; ;; ;; ;; Code: cursor positioning and closely allied stuff

prscrollrgn = *
    ; The sequence "ESC [ ... r" which is a sequence to set the scroll
    ; region.  Takes two parameters, which are row numbers for top and
    ; bottom.  They must be different.  They're numbered from 1; a
    ; value of zero means take the extreme.  This operation also results
    ; in moving to the top row of the screen.

    ; Get the parameter values, and "sanitize" them, with the resulting
    ; values ending up in atmp and atmp+1.
    lda numbufh ; first parameter, high half
    bne prscrollrgn__bad ; if first parameter >= 256: bad command, no action
    lda numbufl ; first parameter, low half
    bmi prscrollrgn__bad ; if first parameter >= 128: bad command, no action
    bne prscrollrgn__tnonz
    lda #1 ; if first parameter == 0: use 1
prscrollrgn__tnonz = *
    cmp textsz
    bpl prscrollrgn__bad ; first parameter >= bottom row: bad cmd, no action
    sta atmp ; put the first parameter (top) in atmp
    dec atmp ; and adjust it from 1-based to 0-based
    lda numbufn ; is there even a second parameter?
    beq prscrollrgn__bmax ; no second parameter: use max
    lda numbufh+1 ; second parameter, high half
    bne prscrollrgn__bmax ; second parameter >= 256: use max
    lda numbufl+1 ; second parameter, low half
    bmi prscrollrgn__bmax ; second parameter >= 128: use max
    beq prscrollrgn__bmax ; second parameter == 0: use max
    cmp textsz
    bmi prscrollrgn__bgot ; second parameter < max: no cap
prscrollrgn__bmax = *
    ; second parameter is 0 or >= max: use max
    lda textsz
prscrollrgn__bgot = *
    sta atmp+1 ; put the second parameter (bottom) in atmp+1
    dec atmp+1 ; and adjust it from 1-based to 0-based

    ; Now that we have the top & bottom row numbers (in atmp in 0-based
    ; form), check that they're in the right relation to one another.
    lda atmp
    cmp atmp+1
    bmi prscrollrgn__rok ; top row number < bottom: ok, do it
    ; If top row number >= bottom, the command is invalid, don't do anything.
prscrollrgn__bad = *
    rts
prscrollrgn__rok = *
    ; We have valid row numbers, now store them.
    lda atmp
    sta scroll_top
    lda atmp+1
    sta scroll_bot

    ; Now move the cursor to home position
    jsr cursor_hide ; hide it so we can move it
    jmp cursor_to_home ; move it and return

    ; prleftedge: move the cursor to the left edge.
prleftedge = *
    jsr cursor_hide ; hide the cursor so we can move it safely
    lda #0
    sta cursor_col ; leftmost column
    sta cursor_defm ; cancel a deferred movement, not needed now
    jmp cursor_figure ; recompute cursor_ptr & return

    ; prnextline: move down, possibly scrolling, and move to left edge
prnextline = *
    jsr cursor_hide ; hide the cursor so we can move it safely
    lda #0
    sta cursor_col ; leftmost column
    sta cursor_defm ; cancel a deferred movement, not needed now
    beq prindex_raw ; note: branch is always taken

    ; prindex: move down one line, possibly scrolling
    ; prindex_raw: the same thing with some of its extra operations stripped
    ; out, for when they're already done
prindex = *
    jsr cursor_hide ; hide the cursor so we can move it safely
    lda #0
    sta cursor_defm ; cancel a deferred movement, not needed now
prindex_raw = *
    lda cursor_row ; Look for current row relative to the bottom of the scroll
                   ; region: that determines if we scroll, or move, or not.
    cmp scroll_bot
    bne prindex__noscroll
    jsr scroll_shift_up ; need to scroll
    jmp cursor_figure ; recompute cursor_ptr and return
prindex__noscroll = *
    ; Not at the bottom of scroll region, so don't scroll; but at bottom
    ; of screen, don't even move.
    cmp textszm1
    bpl prindex__return
    inc cursor_row ; next row
prindex__return = *
    jmp cursor_figure ; recompute cursor_ptr and return

    ; prrindex(): move up one line, possibly scrolling
prrindex = *
    jsr cursor_hide ; hide the cursor so we can move it safely
    lda #0
    sta cursor_defm ; cancel a deferred movement, not needed now
    lda cursor_row ; Look for current row relative to the bottom of the scroll
                   ; region: that determines if we scroll, or move, or not.
    cmp scroll_top
    bne prrindex__noscroll
    jsr scroll_shift_down ; need to scroll
    jmp cursor_figure ; recompute cursor_ptr & return
prrindex__noscroll = *
    ; Not at the top of scroll region, so don't scroll; but at top of 
    ; screen, don't even move.
    tax ; TAX here is short for "CMP #0"
    beq prrindex__return
    dec cursor_row ; previous row
prrindex__return = *
    jmp cursor_figure ; recompute cursor_ptr & return
    
    ; prlf: Handle the LF character ('\n')
prlf = *
    lda modeflags2 ; check for line feed / newline mode
    and #MODEFLAGS2_LFNL
    bne prlf__lfnl
    jmp prindex ; not in lf/nl mode: just move down one
prlf__lfnl = *
    jmp prnextline ; in lf/nl mode: move down one & to the left

    ; prcr: Handle the CR character ('\r')
prcr = *
    jmp prleftedge

    ; prtab: display a HT ('\t') character by moving right to the next
    ; tab stop or right edge of screen.
prtab = *
    ; Note that unlike most other cursor movement operations, prtab() does
    ; not clear cursor_defm.
    jsr cursor_hide ; hide the cursor so we can move it safely
    lda cursor_col ; see if we're in rightmost column of screen
    cmp #SCREEN_WIDTH-1
    beq prtab__done ; stop moving if we are
    inc cursor_col ; move right, because we can
    ldx cursor_col ; see if we got to a tab stop
    lda tabstops,x
    beq prtab ; no, not a tab stop: keep moving
prtab__done = *
    jmp cursor_figure ; recompute cursor_ptr & return

    ; prbackspace: Implement the BS ('\010') character by moving left one
    ; character position, stopping at the left edge of the screen
prbackspace = *
    lda #0
    sta cursor_defm ; cancel a deferred movement
    jsr cursor_hide ; hide the cursor so we can move it safely
    lda cursor_col
    beq prbackspace__done ; do nothing if we're already at the left edge
    dec cursor_col ; left one character position
    lda cursor_ptr ; left two bytes
    sec
    sbc #2
    sta cursor_ptr
    lda cursor_ptr+1
    sbc #0
    sta cursor_ptr+1
prbackspace__done = *
    rts

    ; prcleartabs - clear one or all tab stops a la the "ESC [ ... g" escape
    ; sequence.  Only honors its first parameter.  Only accepts parameter
    ; values of 0 and 3.
prcleartabs = *
    lda numbufh
    bne prcleartabs__nop ; value >= 256, thus not 0 or 3
    lda numbufl
    beq prcleartabs__one ; value == 0, thus clear a single tab stop
    cmp #3
    beq prcleartabs__all ; value == 3, thus clear all tab stops
prcleartabs__nop = *
    ; not a recognized parameter value, so do nothing
    rts
prcleartabs__one = *
    ; ^[[0g or ^[[g - clear tab stop at current cursor position
    ldx cursor_col ; current cursor position
    lda #0 ; zero to clear the tab stop
    sta tabstops,x ; clear that particular one
    rts
prcleartabs__all = *
    ; ^[[3g - clear all tab stops
    ldx #SCREEN_WIDTH-1 ; start with the rightmost possible tab stop
    lda #0 ; clearing all of them by putting zero in 'tabstops'
prcleartabs__loop = *
    sta tabstops,x ; clear this one
    dex ; go left
    bpl prcleartabs__loop ; repeat while there are any left
    rts

    ; initial_tabstops() - initialize the 'tabstops' array with tab stops
    ; at positions divisible by 8
initial_tabstops = *
    ldx #79 ; position counter
    lda #$80 ; tab selection rotor
initial_tabstops__loop = *
    pha ; save the rotor value
    and #1 ; its first bit will determine if it's a tab stop or not
    sta tabstops,x
    pla ; get back the rotor value
    pha ; rotate right by 1 bit, within the 8 bit byte
    ror a
    pla
    ror a
    dex ; count down positions
    bpl initial_tabstops__loop ; continue as long as there's more to do
    rts

    ; prdelchars - delete some characters right of the cursor s a la the
    ; "ESC [ ... P" escape sequence.  Only honors its first parameter,
    ; which is stored in numbufl and numbufh.  This also munges the parameter
    ; as a convenient way to count how much is really left to do.
    ; If parameter is zero, deletes one character, like xterm does.
    ; YYY prdelchars() is slow because it moves each character one cell
    ; at a time, thus taking O(n^2) time; see 2015/Sep/05 notes for reason
    ; this could be troublesome.
prdelchars = *
    ; Hide the blinking cursor since we're going to modify TEXTRAM
    jsr cursor_hide
    ; Mangle the parameter to count the actual number left to do.
    lda numbufh
    bne prdelchars__huge ; >= 256, shorten to 80
    lda numbufl
    beq prdelchars__zero ; == 0, extend to 1
prdelchars__loop = *
    ; numbufl has the number of characters left to do, and it's at least one;
    ; do one character
    jsr prdelchar
    ; count that character & see if there are any left
    dec numbufl
    bne prdelchars__loop ; there are some left
    rts ; none left: done
prdelchars__huge = *
    ; parameter is >= 256, shorten it to 80 and let numbufh be ignored
    lda #SCREEN_WIDTH
prdelchars__set_and_go = *
    sta numbufl
    jmp prdelchars__loop
prdelchars__zero = *
    ; parameter is zero, use default value of 1
    lda #1
    bne prdelchars__set_and_go ; branch always taken

    ; prdelchar() - delete one character, the one that's under the cursor,
    ; moving all the characters right of it, left by one.  Fill in the
    ; rightmost with a space, and the attributes of what was there before.
prdelchar = *
    ; cursor_ptr will point to the current character; copy it to atmp where
    ; we can manipulate it.
    lda cursor_ptr
    sta atmp
    lda cursor_ptr+1
    sta atmp+1
    ; Figure out how many characters there are, right of the cursor
    ; position, to copy.
    lda #SCREEN_WIDTH ; 80
    clc
    sbc cursor_col ; SCREEN_WIDTH-1-cursor_col
    tax
    beq prdelchar__moved ; if there's nothing to move
    ; Now, move X characters left by one, pointed to by atmp.
prdelchar__loop = *
    ; move the two bytes that make up one character
    ldy #2
    lda (atmp),y
    ldy #0
    sta (atmp),y
    ldy #3
    lda (atmp),y
    ldy #1
    sta (atmp),y
    ; now to move right and count it
    lda atmp
    clc
    adc #2
    sta atmp
    lda atmp+1
    adc #0
    sta atmp+1
    dex
    ; are there any more left to do?
    bne prdelchar__loop ; yes: do them
prdelchar__moved = *
    ; All characters have been moved, now clear the rightmost; note that
    ; atmp points at it by now
    ldy #0
    lda #' '
    sta (atmp),y
    ; and set its attributes to the ones we're currently drawing with
    ; (like xterm does)
    iny
    lda prchattr
    sta (atmp),y
    ; all done
    rts

    ; prinschar() - Inserts one character, under the cursor,
    ; moving all the characters right of it, right by one.  Doesn't actually
    ; fill in the character it inserts; it's up to the caller to do that.
prinschar = *
    ; figure out how many characters there are right of the cursor (the
    ; number to move)
    lda #SCREEN_WIDTH ; 80
    clc
    sbc cursor_col ; 80-1-cursor_col - chars to move
    tax ; we'll count the characters to move in X
    beq prinschar__moved ; if there's nothing to move
    ; now figure out the address of the penultimate character; put in atmp
    sec
    sbc #1 ; one less
    asl a ; 2 bytes per character
    clc
    adc cursor_ptr
    sta atmp
    lda #0
    adc cursor_ptr+1
    sta atmp+1
prinschar__loop = *
    ; move the two bytes that make up one character
    ldy #0
    lda (atmp),y
    ldy #2
    sta (atmp),y
    ldy #1
    lda (atmp),y
    ldy #3
    sta (atmp),y
    ; now to move left and count it
    lda atmp ; subtract 2 from atmp
    sec
    sbc #2
    sta atmp
    lda atmp+1
    sbc #0
    sta atmp+1
    dex ; and 1 from X
    ; are there any more left to do?
    bne prinschar__loop ; yes: do them
prinschar__moved = *
    ; all done
    rts

    ; cursor_to_home(): Move the cursor to "home" position, which is
    ; line 1, column 1.  Caller should call cursor_hide() first.
cursor_to_home = *
    ldx #1 ; column
    ldy #1 ; row
    jmp cursor_set_pos ; set and return

    ; cursor_set_pos(): Position the cursor at row Y, column X, as numbered
    ; by the escape codes.  That is, home position is 1, 1.
    ; It can take parameters that are a little out of range, but not
    ; >= 128.
    ; Caller should call cursor_hide() first.
cursor_set_pos = *
    ; Clear any deferred cursor movement that was pending, it no longer is.
    lda #0
    sta cursor_defm
    ; Deal with the X coordinate, which can be in the range 1-80; convert
    ; to 0-based coordinates we'll use later.
    txa
    beq cursor_set_pos__xadjusted ; if X = 0 don't subtract 1
    cmp #SCREEN_WIDTH
    bmi cursor_set_pos__xclipped ; if X < 80 don't clip
    ldx #SCREEN_WIDTH ; clip to X = 80
cursor_set_pos__xclipped = *
    dex ; convert from 1-based to 0-based coordinates
cursor_set_pos__xadjusted = *
    ; Deal with the Y coordinate, the range and meaning for which depend
    ; on the "origin mode" setting.  Convert to absolute 0-based coordinates
    ; we'll use later.
    lda modeflags1
    and #MODEFLAGS1_ORIGIN
    bne cursor_set_pos__omode ; branch if we're in origin mode
    ; Normal mode: Y=1 is top of screen, and Y can be anywhere on screen.
    tya
    beq cursor_set_pos__nmode_yadjusted ; if Y = 0 don't subtract 1
    cmp textsz
    bmi cursor_set_pos__nmode_yclipped ; if Y < textsz don't clip
    ldy textsz ; clip to Y = textsz
cursor_set_pos__nmode_yclipped = *
    dey ; convert from 1-based to 0-based coordinates
cursor_set_pos__nmode_yadjusted = *
    jmp cursor_set_pos__go
cursor_set_pos__omode = *
    ; Origin mode: Y=1 is scroll_top, and Y can't be below scroll_bot
    tya
    beq cursor_set_pos__omode_ydecced ; if Y = 0 don't subtract 1
    dey ; convert from 1-based to 0-based coordinates
    tya
cursor_set_pos__omode_ydecced = *
    clc
    adc scroll_top ; adjust Y position (in A) to scroll region
    cmp scroll_bot ; check it against bottom of scroll region
    bmi cursor_set_pos__omode_yclipped ; if row < scroll_bot, no clip needed
    beq cursor_set_pos__omode_yclipped ; if row = scroll_bot, no clip needed
    lda scroll_bot ; clip row to scroll_bot
cursor_set_pos__omode_yclipped = *
    tay ; now put the Y position back in the Y register
cursor_set_pos__go = *
    ; now X & Y hold absolute coordinates in which to put the cursor
    stx cursor_col
    sty cursor_row
    jmp cursor_figure ; compute new cursor_ptr & return

    ; prerase(): Handle the "ED" sequences, ^[[...J which erase half or
    ; all the screen.  Uses all registers, and atmp.
prerase = *
    lda numbufh
    bne prerase__return ; parameter >= 256 is not recognized, do nothing
    lda numbufl
    beq prerase__0 ; parameter 0 erases from cursor down
    cmp #1
    beq prerase__1 ; parameter 1 erases from cursor up
    cmp #2
    beq prerase__2 ; parameter 2 erases the whole screen
prerase__return = *
    rts ; out of range parameter does nothing
prerase__2 = *
    ; ^[[2J - erase whole screen
    ; Counts rows, from textsz-1 to 0, blanking each.
    jsr cursor_hide ; hide cursor since we'll draw over it
    ldx textsz ; get the number of rows
prerase__2_pre_loop = *
    dex ; max row is one less than that
prerase__2_loop = *
    lda #$00 ; clear the row attributes of this row
    sta linctl_b,x
    jsr find_row ; point 'atmp' to the row's data
    lda #SCREEN_WIDTH*2 ; blank the whole row
    jsr blankchars
    dex ; next row
    bpl prerase__2_loop ; branch if there are more rows to do
    rts
prerase__1 = *
    ; ^[[1J - erase from cursor up (and left)
    jsr cursor_hide ; hide cursor since we'll draw over it
    ldx cursor_row ; get current row the cursor is in
    jsr find_row ; get pointer to start of that row (into 'atmp')
    ldy cursor_col ; figure out how much of the row to erase
    iny ; include the character under the cursor
    tya
    asl a ; character cells -> bytes, by multiplying by 2
    jsr blankchars ; erase the left part of this row's characters
    txa ; if x > 0 there are more rows to clear
    bne prerase__2_pre_loop ; shared code for deleting those rows upwards
    rts
prerase__0 = *
    ; ^[[0J - erase from cursor down (and right)
    lda cursor_col ; check for upper left corner
    ora cursor_row
    beq prerase__2 ; Special case: ^[[0J from the upper left corner of
                   ; the screen, do the same as ^[[2J.
                   ; Otherwise: It would be almost, but not quite the same.
                   ; And the difference is significant in some cases,
                   ; namely I've seen some "clear" commands do ^[[H^[[J
                   ; instead of ^[[H^[[2J.
    jsr cursor_hide ; hide cursor since we'll draw over it
    lda cursor_ptr ; get cursor position, to erase
    sta atmp
    lda cursor_ptr+1
    sta atmp+1
    lda #SCREEN_WIDTH ; characters to erase: 80 - cursor_col
    sec
    sbc cursor_col
    asl a ; character cells -> bytes, by multiplying by 2
    jsr blankchars ; erase the right part of this row's characters
    ldx cursor_row
    inx ; full row deletion: next row below that
prerase__0_loop = *
    txa ; check whether we're out of rows yet
    cmp textsz
    bpl prerase__return ; out of rows, done
    lda #$00 ; clear the row attributes of this row
    sta linctl_b,x
    jsr find_row ; point 'atmp' to the row's data
    lda #SCREEN_WIDTH*2 ; blank the whole row
    jsr blankchars
    inx ; next row
    jmp prerase__0_loop

    ; preraseinline() - ^[[...K - "EL" erase characters within the current line.
    ; Uses all registers, and atmp.
preraseinline = *
    jsr cursor_hide ; hide cursor since we'll draw over it
    lda numbufh
    bne preraseinline__return ; parameter >= 256 is not recognized, do nothing
    lda numbufl
    beq preraseinline__0 ; parameter 0 erases from cursor right
    cmp #1
    beq preraseinline__1 ; parameter 1 erases from cursor left
    cmp #2
    beq preraseinline__2 ; parameter 2 erases the whole line
preraseinline__return = *
    rts ; unrecognized parameter does nothing
preraseinline__2 = *
    ; ^[[2K - erase whole line
    ldx cursor_row ; point 'atmp' at this row's data
    jsr find_row
    lda #SCREEN_WIDTH*2 ; we'll blank the whole row
    jmp blankchars ; and blank them and return
preraseinline__0 = *
    ; ^[[K or ^[[0K - erase from cursor to right edge
    lda cursor_ptr ; point atmp at the cursor position
    sta atmp
    lda cursor_ptr+1
    sta atmp+1
    lda #SCREEN_WIDTH ; now figure out number of characters to blank
    sec
    sbc cursor_col
    asl a ; convert to bytes (2 bytes per character)
    jmp blankchars ; and blank them and return
preraseinline__1 = *
    ; ^[[1K - erase from left edge to cursor
    ldx cursor_row ; point 'atmp' at this row's data
    jsr find_row ; point 'atmp' at this row's data
    lda cursor_col ; figure out number of characters to blank
    clc
    adc #1 ; including the character the cursor is on
    asl a ; convert to bytes (2 bytes per character)
    jmp blankchars ; and blank them and return

    ; prinsertline() - ^[[...L - "IL" insert $1 lines at cursor, moving
    ; any lower lines down
prinsertline = *
    jsr cursor_hide ; hide cursor since we'll draw over it
    jsr adj_param_1_127 ; find how many rows to do
    sta btmp ; store it for later in btmp[0]
    lda scroll_top ; store current top of scroll region in btmp[1]
    sta btmp+1
    lda cursor_row ; look at current row
    cmp scroll_top ; is it above the scroll region?
    bmi prinsertline__end ; if so, do nothing
    cmp scroll_bot ; is it below the scroll region?
    bmi prinsertline__go ; if not, do our thing
prinsertline__end = *
    ; this is the end of prinsertline(): clean up & return
    lda btmp+1 ; restore the scroll region
    sta scroll_top
    jmp cursor_figure ; recompute cursor_ptr & return
prinsertline__go = *
    sta scroll_top ; scroll between cursor & bottom of region
prinsertline__loop = *
    jsr scroll_shift_down ; insert line & scroll
    dec btmp ; count one more line done
    bne prinsertline__loop ; repeat while there are more to do
    beq prinsertline__end ; note: branch always taken

    ; prdeleteline() - ^[[...M - "DL" delete $1 lines at cursor, moving
    ; any lower lines up
    ; YYY prinsertline() & prdeleteline() are nearly identical and should
    ; perhaps be consolidated into a single function
prdeleteline = *
    jsr cursor_hide ; hide cursor since we'll draw over it
    jsr adj_param_1_127 ; find how many rows to do
    sta btmp ; store it for later in btmp[0]
    lda scroll_top ; store current top of scroll region in btmp[1]
    sta btmp+1
    lda cursor_row ; look at current row
    cmp scroll_top ; is it above the scroll region?
    bmi prdeleteline__end ; if so, do nothing
    cmp scroll_bot ; is it below the scroll region?
    bmi prdeleteline__go ; if not, do our thing
prdeleteline__end = *
    ; this is the end of prinsertline(): clean up & return
    lda btmp+1 ; restore the scroll region
    sta scroll_top
    jmp cursor_figure ; recompute cursor_ptr & return
prdeleteline__go = *
    sta scroll_top ; scroll between cursor & bottom of region
prdeleteline__loop = *
    jsr scroll_shift_up ; insert line & scroll
    dec btmp ; count one more line done
    bne prdeleteline__loop ; repeat while there are more to do
    beq prdeleteline__end ; note: branch always taken

    ; prcursorup() - ^[[...A - "CUU" cursor up $1 lines to top margin
    ; but not if at scroll_top.
prcursorup = *
    jsr cursor_hide ; hide the cursor so we can move it
    lda #0 ; clear a "deferred cursor move"
    sta cursor_defm
    jsr adj_param_1_127 ; get parameter (count)
    tax ; keep it in the X register
prcursorup__loop = *
    lda cursor_row ; look at where we are
    beq prcursorup__done ; if we're at top edge of screen, we're done
    cmp scroll_top
    beq prcursorup__done ; if we're *at* top edge of scroll region, we're done
    dec cursor_row ; move the cursor
    dex ; count our moves
    bne prcursorup__loop ; repeat as called for
prcursorup__done = *
    jmp cursor_figure ; recompute cursor_ptr and return

    ; prcursordown() - ^[[...B - "CUD" cursor down $1 lines to bottom margin
    ; but not if at scroll_bot.
prcursordown = *
    jsr cursor_hide ; hide the cursor so we can move it
    lda #0 ; clear a "deferred cursor move"
    sta cursor_defm
    jsr adj_param_1_127 ; get parameter (count)
    tax ; keep it in the X register
prcursordown__loop = *
    lda cursor_row ; look at where we are
    cmp textszm1
    beq prcursordown__done ; if we're at bottom edge of screen, we're done
    cmp scroll_bot
    beq prcursordown__done ; if we're *at* bottom edge of scroll rgn: we're done
    inc cursor_row ; move the cursor
    dex ; count our moves
    bne prcursordown__loop ; repeat as called for
prcursordown__done = *
    jmp cursor_figure ; recompute cursor_ptr and return

    ; prcursorright() - ^[[...C - "CUF" cursor right $1 cols to right margin
prcursorright = *
    jsr cursor_hide ; hide the cursor so we can move it
    lda #0 ; clear a "deferred cursor move"
    sta cursor_defm
    jsr adj_param_1_127 ; get parameter (count)
    tax ; keep it in the X register
prcursorright__loop = *
    lda cursor_col
    cmp #SCREEN_WIDTH-1
    beq prcursorright__done ; at right edge of screen we're done
    inc cursor_col ; move the cursor
    dex ; count our moves
    bne prcursorright__loop ; repeat as called for
prcursorright__done = *
    jmp cursor_figure ; recompute cursor_ptr and return

    ; prcursorleft() - ^[[...D - "CUB" cursor left $1 cols to left margin
prcursorleft = *
    jsr cursor_hide ; hide the cursor so we can move it
    lda #0 ; clear a "deferred cursor move"
    sta cursor_defm
    jsr adj_param_1_127 ; get parameter (count)
    tax ; keep it in the X register
prcursorleft__loop = *
    lda cursor_col
    beq prcursorleft__done ; at left edge of screen we're done
    dec cursor_col ; move the cursor
    dex ; count our moves
    bne prcursorleft__loop ; repeat as called for
prcursorleft__done = *
    jmp cursor_figure ; recompute cursor_ptr and return

    ; prcursorpos() - position the cursor by either of the following
    ; sequences:
    ;       ^[[...H - "CUP"
    ;       ^[[...f - "HVP"
    ; in either case it takes two parameters: line number & column number
prcursorpos = *
    jsr cursor_hide ; hide the cursor so we can move it
    jsr adj_param_1_127 ; get first parameter (line) if supplied
    tay ; Y register is where cursor_set_pos() wants it
    bne prcursorpos__goty
    ldy #1 ; if line number is zero or missing, that means line one
prcursorpos__goty = *
    ldx #1 ; if column number is zero or missing, that'll mean column one
    lda numbufn ; see if there's a second parameter (column)
    beq prcursorpos__gotx ; branch if it's missing
    lda numbufh+1 ; look at high half of second parameter
    bne prcursorpos__xbig ; branch if x >= 256
    lda numbufl+1 ; look at low half of second parameter
    beq prcursorpos__gotx ; branch if it's zero or missing
    bmi prcursorpos__xbig ; branch if x >= 128
    tax ; X is where cursor_set_pos() wants it
prcursorpos__gotx = *
    ; Now Y holds the line number & X holds the column number.
    ; they might be out of range but not by too much for cursor_set_pos()
    ; to handle.
    jmp cursor_set_pos
prcursorpos__xbig = *
    ; the second parameter is >= 127, fill in SCREEN_WIDTH, the maximum
    ; meaningful value
    ldx #SCREEN_WIDTH
    ; now go for it
    jmp cursor_set_pos

;; ;; ;; ;; Code: Display attributes

prattcode = *
    ; prattcode() - Subroutine that takes a numeric parameter from
    ; "ESC [ ... m" and applies it.
    ; Input: accumulator.
    ; Output: prchattr and prchinv.
    ; Uses: A, X, Y, atmp
    tax ; TAX is here short for CMP #0
    bmi prattcode_nop ; codes >= 128: not recognized
    cmp #50
    bpl prattcode_nop ; codes >= 50: not recognized
    tax ; use code to lookup in 8- and 16-bit wide tables with X & Y
    asl a
    tay
    lda prattcode_jmp_tbl,y ; jump table, 16 bits wide, low half
    sta atmp
    lda prattcode_jmp_tbl+1,y ; jump table, 16 bits wide, high half
    sta atmp+1
    lda prattcode_arg_tbl,x ; argument table, 8 bits wide
    jmp (atmp) ; and go!

    ; prattcodes() - handle ^[[...m, which VT102 manual calls "set graphical
    ; rendition" and which sets various character properties.
prattcodes = *
    ; Process "ESC [ ... m": turn the numeric parameters into attribute
    ; changes.
    lda #0 ; start with first parameter in order given
prattcodes__loop = *
    pha ; stash that counter for later
    tax ; use it to index
    lda numbufh,x ; look at high byte of the parameter
    bne prattcodes__1done ; >= 256 is meaningless and ignored
    lda numbufl,x ; get low byte of the parameter
    jsr prattcode ; apply it to prchattr & prchinv
prattcodes__1done = *
    ; So, we've done one parameter, now on to the next, if there is one.
    pla ; get the counter back
    cmp numbufn ; see if there are numbers left
    bpl prattcodes__return ; if not: then all is done
    ; There's another one
    clc
    adc #1
    jmp prattcodes__loop
prattcodes__return = *
    rts


prattcode_default = *
    ; prattcode_default - end of prattcode() for code 0, which takes the
    ; attributes back to their default values
    lda #0 ; no inverse video
    sta prchinv
    lda #DEFFG ; default foreground, black background, no bright or underline
    sta prchattr
    sta prchattru
    lda #DEFFG<<4 ; the same inverted
    sta prchattri
    rts

prattcode_nop = *
    ; prattcode_nop - end of prattcode() for codes that we don't recognize
    ; or don't implement or that have no effect.
    rts

prattcode_setbit = *
    ; prattcode_setbit - end of prattcode() for codes 1, 4, which set
    ; a specific attribute bit to 1.  The bit is in the accumulator.
    ; It's not part of the colors so it doesn't participate in inverse video.
    tax ; store the bit for multiple use
    ora prchattru
    sta prchattru
    txa
    ora prchattri
    sta prchattri
    txa
    ora prchattr
    sta prchattr
    rts

prattcode_clrbit = *
    ; prattcode_clrbit - end of prattcode() for codes 22, 24, which set
    ; a specific attribute bit to 0.  The bit is in the accumulator.
    ; It's not part of the colors so it doesn't participate in inverse video.
    eor #$ff ; flip from a single bit set to a single bit clear
    tax ; store the bit for multiple use
    and prchattr
    sta prchattr
    txa
    and prchattru
    sta prchattru
    txa
    and prchattri
    sta prchattri
    rts

prattcode_inverse = *
    ; prattcode_inverse - end of prattcode() for codes 7, 27, which enable
    ; or disable inverse video.  When called, the accumulator will be
    ; zero to disable, one to enable.
    cmp prchinv ; is anything even changing?
    beq prattcode_nop ; nope, nothing happens
    sta prchinv ; change the inverse state
    tax ; and look up the corresponding inverted/uninverted attributes byte
    lda prchattru,x
    sta prchattr ; and store it
    rts ; done

prattcode_color = *
    ; prattcode_color - end of prattcode() for codes 30-37, 39, 40-47, 49,
    ; which set foreground or background color.  When called,
    ; the accumulator will hold:
    ;       $07 - color value
    ;       $80 - set for background, clear for foreground
    ; It has to take into account inverse video, which makes it
    ; more complicated than it would be otherwise.  Uses atmp for temporary
    ; storage.
    asl a ; test for background (and get rid of that bit)
    bcs prattcode_color__bg
    ; doing foreground
    lsr a ; having got rid of the inverse bit, put others back where they go
    sta atmp ; stash the value for later use
    lda prchattru ; replace the FG field of the uninverted attributes byte
    and #$f8
    ora atmp
    sta prchattru
    lda atmp ; and put it in the BG field of the inverted attributes byte
    asl a
    asl a
    asl a
    asl a
    sta atmp
    lda prchattri
    and #$8f
    ora atmp
    sta prchattri
    jmp prattcode_color__sel
prattcode_color__bg = *
    ; doing background
    lsr a ; having got rid of the inverse bit, put others back where they go
    sta atmp ; stash the value for later use
    lda prchattri ; replace the FG field of the inverted attributes byte
    and #$f8
    ora atmp
    sta prchattri
    lda atmp ; and put it in the BG field of the uninverted attributes byte
    asl a
    asl a
    asl a
    asl a
    sta atmp
    lda prchattru
    and #$8f
    ora atmp
    sta prchattru
prattcode_color__sel = *
    ; having changed the "inverted" and "uninverted" attributes byte, pick
    ; the one that's in current use
    ldx prchinv
    lda prchattru,x
    sta prchattr
    rts

    ; ESC # 3 - make the current row: double width, double height, upper half
prescps_rowdhu = *
    ldx cursor_row ; identify current row
    lda linctl_b, x ; get its row attributes, to change them
    ora #$90 ; double height double width
    and #$bf ; upper half
    sta linctl_b, x ; change row attributes
    rts

    ; ESC # 4 - make the current row: double width, double height, lower half
prescps_rowdhl = *
    ldx cursor_row ; identify current row
    lda linctl_b, x ; get its row attributes, to change them
    ora #$d0 ; double height double width lower half
    sta linctl_b, x ; change row attributes
    rts

    ; ESC # 5 - make the current row: single width, single height (normal)
prescps_rowswh = *
    ldx cursor_row ; identify current row
    lda linctl_b, x ; get its row attributes, to change them
    and #$2f ; single height single width
    sta linctl_b, x ; change row attributes
    rts

    ; ESC # 6 - make the current row: double width, single height
prescps_rowdw  = *
    ldx cursor_row ; identify current row
    lda linctl_b, x ; get its row attributes, to change them
    ora #$10 ; double width
    and #$3f ; single height
    sta linctl_b, x ; change row attributes
    rts

    ; prnarrowrow(): Call after the current row has been set to double width
    ; double height, to make it single width, double height.  There is no
    ; escape sequence for this one, not only because the VT102 had none;
    ; but also because VTJ-1 uses it to identify the special menu mode.
prnarrowrow = *
    ldx cursor_row ; identify current row
    lda linctl_b, x ; get its row attributes, to change them
    and #$e0 ; no longer double width
    sta linctl_b, x ; change row attributes
    rts

    ; prcontrol_cset: Control codes 14 & 15 select character sets G1 and G0
    ; respectively.
prcontrol_cset = *
    and #1 ; figure out which character set it is
    eor #1
    sta cset_sel ; and record the selection
    tax
    lda cset_g0, x ; and pick up the G0 or G1 character set
    sta cset ; and record it as the current one
    rts

decode_charset = *
    ; given a single character that identifies a character set, decode it
    ; into the form used in 'cset'.
    ; Sets the carry flag on an unrecognized input code.
    ; Looks up characters in the list cset_lst_*.
    ldx #0 ; list index
decode_charset__loop = *
    ldy cset_lst_char, x ; check for end which'll have char code >= 128
    bmi decode_charset__notfound
    cmp cset_lst_char, x ; check for a match
    beq decode_charset__found
    ; no match yet, but we could try another value in the list
    inx
    jmp decode_charset__loop
decode_charset__notfound = *
    ; No match for this character was found.  Set carry to indicate that
    ; fact, and return
    sec
    rts
decode_charset__found = *
    ; A match for this character was found.  Return the code taken from
    ; the list, and clear carry to indicate success.
    lda cset_lst_code, x
    clc
    rts

;; ;; ;; ;; Code: Mode setting and special commands

IF ENABLE_DEVEL
prdevel = *
    ; The sequence "ESC [ ... |" which is a private sequence of VTJ-1
    ; for development / debug use.  When I'm done I should probably remove
    ; or disable this.
    ; The sequence's first numeric parameter determines its operation:
    ;       ^[[23| - code timing report
    ;       ^[[46| - clear video error
    ;       ^[[69;addr| - read from memory
    ;       ^[[92;addr;byte| - write to memory
    ;       ^[[115;int| - echo an integer in hex
    ;       ^[[138| - identify rows on screen
    ;       ^[[161| - expose keycodes
    ;       ^[[184;...| - fake keycodes
    ;       ^[[207;value| - programmable delay
    ;       ^[[253;addr| - execute code at given address
    lda numbufh
    bne prdevel_bogus ; command >= 256, not implemented
    lda numbufl
IF ENABLE_DEVEL_2
    cmp #23
    beq prdevel_rept ; command 23, code timing report
    cmp #46
    beq prdevel_cve ; command 46, clear video error
ENDC ; ENABLE_DEVEL_2
    cmp #69
    beq prdevel_read ; command 69, read from memory
    cmp #92
    beq prdevel_write ; command 92, write to memory
IF ENABLE_DEVEL_2
    cmp #115
    beq prdevel_echoint ; command 115, echo integer in hexadecimal
    cmp #138
    beq prdevel_idrows ; command 138, identify rows in text RAM
    cmp #161
    beq prdevel_keycodes ; command 161, expose keycodes
ENDC ; ENABLE_DEVEL_2
    cmp #184
    beq prdevel_fakekeys_j ; command 184, insert fake keycodes
IF ENABLE_PROGDELAY
    cmp #207
    beq prdevel_progdelay_j ; command 207, programmable delay
ENDC ; ENABLE_PROGDELAY
    cmp #253
    beq prdevel_run_j ; command 253, execute code at address
prdevel_bogus = * ; command not implemented
    lda #'C'
    jsr txchar
    lda #'N'
    jsr txchar
    lda #'I'
    jsr txchar
    lda #'!'
    jmp txchar ; and that will return for us
IF ENABLE_DEVEL_2
prdevel_rept = * ; command 23, code timing report
    jmp devel_rept
prdevel_cve = * ; command 46, clear video error
    lda #V_CTL_CLEAR
    sta V_CTL
    rts
ENDC ; ENABLE_DEVEL_2
prdevel_read = * ; command 69, read a byte from memory
    ldx numbufn ; get the last parameter, which should be the second
    lda numbufl,x
    sta atmp
    lda numbufh,x
    sta atmp+1
    ldy #0 ; read that location in memory
    lda (atmp),y
    jmp txhex ; transmit it in hex, and return
prdevel_write = * ; command 92, write a byte to memory
    lda numbufn ; there should be three integers
    cmp #2
    bne prdevel_bogus ; if not, fail
    lda numbufl+1 ; get address
    sta atmp
    lda numbufh+1
    sta atmp+1
    lda numbufl+2 ; get value
    ldy #0 ; and write it
    sta (atmp),y
    rts ; that's all
IF ENABLE_DEVEL_2
prdevel_echoint = * ; command 115, echo a 16 bit integer in hex
    ldx numbufn ; get the last parameter, which should be the second
    lda numbufh,x ; first byte
    jsr txhex ; transmit it
    ldx numbufn ; second byte
    lda numbufl,x
    jmp txhex ; transmit it and return
prdevel_keycodes = * ; command 161, expose key codes
    lda modeflags1 ; set the bit enabling this debug feature
    ora #MODEFLAGS1_KEYCODES
    sta modeflags1
    rts ; and that's all
ENDC ; ENABLE_DEVEL_2
prdevel_fakekeys_j = * ; command 184, insert fake key codes
    jmp prdevel_fakekeys
IF ENABLE_PROGDELAY
prdevel_progdelay_j = * ; command 207, set programmable delay
    jmp prdevel_progdelay
ENDC ; ENABLE_PROGDELAY
prdevel_run_j = * ; command 253, execute code
    jmp prdevel_run
IF ENABLE_DEVEL_2
prdevel_idrows = * ; command 138, identify rows in text RAM
    ; Initialize state:
    ;   atmp - address of start of row
    ;   btmp[0] - upper digit value of row number (0-9)
    ;   btmp[1] - lower digit value of row number (0-9)
    ;   X - rows left to do
    jsr cursor_hide ; hide cursor since we're going to modify TEXTRAM
    lda #TEXTRAM&255
    sta atmp
    lda #TEXTRAM>>8
    sta atmp+1
    lda #0
    sta btmp
    sta btmp+1
    ldx #MAX_STORED_ROW+1
prdevel_idrows__loop = *
    ; Main loop: store info for another row
    ldy #0 ; start with first byte
    lda btmp ; upper digit value
    clc
    adc #'0' ; convert to ASCII
    sta (atmp),y ; store it to screen
    iny
    lda #7 ; white on black
    sta (atmp),y ; store it on screen
    iny
    lda btmp+1 ; lower digit value
    clc
    adc #'0' ; convert to ASCII
    sta (Atmp),y ; store it on screen
    iny
    lda #7 ; white on black
    sta (atmp),y ; store it on screen
    ; Point to next row & count it
    lda #160 ; atmp += 160
    clc
    adc atmp
    sta atmp
    lda atmp+1
    adc #0
    sta atmp+1
    inc btmp+1 ; increment the decimal integer in btmp
    lda btmp+1
    cmp #10
    bne prdevel_idrows__incremented
    lda #0 ; carry within the decimal integer in btmp
    sta btmp+1
    inc btmp
prdevel_idrows__incremented = *
    ; any more rows?
    dex
    bne prdevel_idrows__loop ; yes
    rts ; no
ENDC ; ENABLE_DEVEL_2
prdevel_fakekeys = *
    ; Insert one or more "fake" bytes into inbuf as if they came from
    ; the keyboard.
    ldx #1 ; index into numbuf, right after the command
prdevel_fakekeys__loop = *
    txa ; see if there's another parameter in numbuf
    cmp numbufn
    bmi prdevel_fakekeys__doit ; x < numbufn: not the last one yet
    beq prdevel_fakekeys__doit ; x = numbufn: is the last one
    ; we've run out of parameters; we're done
    rts
prdevel_fakekeys__doit = *
    ; put the parameter on inbuf; takes for granted that inbuf isn't full
    ; and isn't going to become full, if it does happen then weird results
    ; will occur.  Making such a broad assumption is not a good idea in
    ; general but this is just a test tool.
    lda numbufl,x ; take the parameter value (truncated to 8 bits)
    sei ; disable interrupts so we can safely manipulate inbuf
    ldy inbufi ; see where in the buffer we should put things
    sta inbufl,y ; store the byte into the buffer
    lda #INBUF_ET_PS2CH ; and the type of event too
    sta inbufh,y
    iny ; now change buffer position
    tya
    and #INBUFSZ-1
    sta inbufi
    lda #1 ; and at any rate, buffer is no longer empty
    sta inbufnotempty
    cli ; re-enable interrupts now that we're done adding this byte
    inx ; now see about the next byte
    jmp prdevel_fakekeys__loop
prdevel_run = * ; command 253, execute code
    ldx numbufn ; get the last parameter, which should be the second
    lda numbufl,x
    sta atmp
    lda numbufh,x
    sta atmp+1
    jmp (atmp)
IF ENABLE_PROGDELAY
prdevel_progdelay = *
    ; Set 'progdelay', a programmable delay counter.  The delay happens
    ; before waiting for and accepting a new input event.
    ldx numbufn ; get the last parameter, which should be the second
    lda numbufl,x
    sta progdelay
    lda numbufh,x
    sta progdelay+1
    rts
ENDC ; ENABLE_PROGDELAY
ENDC ; ENABLE_DEVEL

    ; ESC # 8 - fill the whole screen with capital letter E in the default
    ; foreground & background colors.  Yes, that's a thing.
    ; Thus, we're going to fill all of TEXTRAM with the following two byte
    ; sequence:
    ;   $45 - 'E'
    ;   DEFFG - default foreground, black background, no underline
prescps_eeeee = *
    jsr cursor_hide ; hide the blinking cursor since we modify TEXTRAM
    lda #TEXTRAM&255 ; point to TEXTRAM in atmp
    sta atmp
    lda #TEXTRAM>>8
    sta atmp+1
    ldx #32 ; number of 32 byte pages to fill
    ldy #0 ; pointer within those pages
prescps_eeeee__loop = *
    lda #$45 ; 'E' byte
    sta (atmp),y
    iny
    lda #DEFFG ; attributes byte
    sta (atmp),y
    iny
    bne prescps_eeeee__loop ; next bytes within page
    inc atmp+1 ; next page
    dex
    bne prescps_eeeee__loop ; more pages
    rts

    ; prmode(): Handle setting or resetting one or more mode flags from
    ; an escape sequence.  Inputs:
    ;   accumulator - 'h' for set, 'l' for reset
    ;   esc_lbvar - ESC_LBVAR_{NORMAL,QUES,EQUAL} to indicate private mode
    ;   numbuf* - The parameters themselves.
    ; Works by stepping through 'prmode_lst'.  Uses 'atmp'.
prmode = *
    sta prmodesr ; record 'h' or 'l' for set or reset
    ; Start going through the parameters.  In practice there will
    ; usually be just one, but there can be multiple.
    ldx #0 ; counts through the parameters
prmode__ploop = *
    lda numbufh,x ; look at high byte of parameter value
    bne prmode__pnext ; if parameter >= 256, it is not recognized & is ignored
    ldy #255 ; counts through possible options as we go through them
             ; (zero is where we really care to start, but we're going to
             ; preincrement it in a moment)
prmode__onext = *
    iny ; next entry in prmode_lst_*
    ; See if prmode_lst_*,y is the right option to go with what we've
    ; got in numbuf*,x.
    lda prmode_lst_lbvar,y ; see what it's for as to private/standard mode
    cmp #ESC_LBVAR_INVALID ; and see if it's the end
    beq prmode__pnext ; branch if it's the end, no matches for this parameter
    cmp esc_lbvar
    bne prmode__onext ; branch if it's wrong about private/standard mode
    lda prmode_lst_value,y ; see what particular parameter value it's for
    cmp numbufl,x ; is it this one?
    bne prmode__onext ; branch if it's not this parameter value
    ; Ok.  It's a match!  We're going to call the set or reset function, as
    ; appropriate, with the right parameter.
    lda prmodesr
    cmp #'l' ; is it L (reset?)
    beq prmode__isreset
    ; it's 'h' (set); copy the set proc into atmp
    lda prmode_lst_set0,y
    sta atmp
    lda prmode_lst_set1,y
    sta atmp+1
    jmp prmode__gotatmp
prmode__isreset = *
    ; it's 'l' (reset); copy the reset proc into atmp
    lda prmode_lst_reset0,y
    sta atmp
    lda prmode_lst_reset1,y
    sta atmp+1
prmode__gotatmp = *
    ; 'atmp' has been set up with the code to call to handle this parameter.
    ; Call it with the right accumulator value as found in the table.  Oh,
    ; and save X, we'll want it later.
    txa
    pha
    lda prmode_lst_acc,y
    jsr atmp_indirector ; equivalent to "jsr (atmp)" if we could do that
    pla ; and now get X back the way it was
    tax
prmode__pnext = *
    ; That parameter has been handled (or ignored); do the next one if any.
    txa ; for checking in a moment
    inx ; next one
    cmp numbufn ; was the last one, the last one?
    bne prmode__ploop ; nope, do the next
    rts ; yup, done

    ; modeflags1_set: Set a bit or bits (found in the accumulator) in
    ; 'modeflags1'.  Used in the implementation of "ESC [ ... h"
modeflags1_set = *
    ora modeflags1 ; combine with what's already there
    sta modeflags1
    rts

    ; modeflags1_reset: Reset a bit or bits (found in the accumulator) in
    ; 'modeflags1'.  Used in the implementation of "ESC [ ... h"
modeflags1_reset = *
    eor #$ff ; flip the bits
    and modeflags1 ; combine with what's already there
    sta modeflags1
    rts

    ; modeflags2_set: Set a bit or bits (found in the accumulator) in
    ; 'modeflags2'.  Used in the implementation of "ESC [ ... h"
modeflags2_set = *
    ora modeflags2 ; combine with what's already there
    sta modeflags2
    rts

    ; modeflags2_reset: Reset a bit or bits (found in the accumulator) in
    ; 'modeflags2'.  Used in the implementation of "ESC [ ... h"
modeflags2_reset = *
    eor #$ff ; flip the bits
    and modeflags2 ; combine with what's already there
    sta modeflags2
    rts

    ; origin_mode_set: Set "origin mode" (ESC [ ? 6 h) which has the side
    ; effect of moving the cursor to home position.
origin_mode_set = *
    lda #MODEFLAGS1_ORIGIN
    ora modeflags1
    sta modeflags1
    jsr cursor_hide ; hide cursor so we can move it
    jmp cursor_to_home ; move cursor to home position & return

    ; origin_mode_reset: Clear "origin mode" (ESC [ ? 6 l) which has the side
    ; effect of moving the cursor to home position.
origin_mode_reset = *
    lda #255-MODEFLAGS1_ORIGIN
    and modeflags1
    sta modeflags1
    jsr cursor_hide ; hide cursor so we can move it
    jmp cursor_to_home ; move cursor to home position & return

IF ENABLE_DEVEL
IF ENABLE_DEVEL_2
devel_rept = *
    ; ESC [ 23 | - report code timing information
    ; much of this depends on the "video timing tester" hardware;
    ; if that's disabled, it'll show zero for those parts of it
    sei ; don't let interrupts happen while we retrieve the counters
    sta VM_CTR_MET ; writing will latch values into the three VM_CTR_* counters
    lda scanline ; and make a copy of the scan line counter
    sta scanlinetmp
    lda scanline+1
    sta scanlinetmp+1
    cli ; we can let interrupts happen now
    jsr txnewline ; put space around our output
    ; now we display our counters
    lda #'S' ; scan line value
    jsr txchar
    lda scanlinetmp+1
    jsr txhex
    lda scanlinetmp
    jsr txhex
    lda #','
    jsr txchar
    lda #'M' ; count of times the condition was met
    jsr txchar
    lda #VM_CTR_MET&255
    sta addr0
    lda #VM_CTR_MET>>8
    sta addr0+1
    jsr txhex4
    lda #','
    jsr txchar
    lda #'E' ; count of clock cycles with interrupts enabled
    jsr txchar
    lda #VM_CTR_IEN&255
    sta addr0
    lda #VM_CTR_IEN>>8
    sta addr0+1
    jsr txhex4
    lda #','
    jsr txchar
    lda #'D' ; count of clock cycles with interrupts disabled
    jsr txchar
    lda #VM_CTR_IDI&255
    sta addr0
    lda #VM_CTR_IDI>>8
    sta addr0+1
    jsr txhex4
    jmp txnewline ; put space around our output, and return
ENDC ; ENABLE_DEVEL_2
ENDC ; ENABLE_DEVEL

    ; prbell() - Handle character code 7, BEL - visible or audible bell
    ; as configured.
prbell = *
IF ENABLE_VISBELL | ENABLE_AUDBELL
    ldx bellon ; see if bell was already on or not
    lda #30 ; ring bell for 1/2 second
    sta bellon
    txa ; if bell was already on, nothing changes
    bne prbell__ret
IF ENABLE_VISBELL
    ; activate visual bell if enabled
    lda #MODEFLAGS1_VISBELL
    bit modeflags1
    beq prbell__visbell_skip
    lda #V_CTL_SETF2|V_CTL_FINV2
    sta V_CTL
prbell__visbell_skip = *
ENDC
IF ENABLE_AUDBELL
    ; activate audible bell if enabled
    lda #MODEFLAGS1_AUDBELL
    bit modeflags1
    beq prbell__audbell_skip
    lda #SYSSFC_BEEP
    sta SYSSFC
prbell__audbell_skip = *
ENDC
ENDC ; ENABLE_VISBELL | ENABLE_AUDBELL
prbell__ret = *
prnop = *
    rts
    ; prnop() is a function that does nothing, and just consists of the
    ; 'rts' at the end of another function, prbell()

    ; prdevatt(): Handle the "Device Attributes (DA)" escape sequences,
    ; ^[[c and ^[[0c.  Any other parameter value ends up as a no-op.
    ; The heavy lifting is done by txdevatt(); all prdevatt() does is
    ; check the parameter.
prdevatt = *
    lda numbufh
    bne prdevatt__nop ; parameter >= 256, thus != 0, thus do nothing
    lda numbufl
    bne prdevatt__nop ; parameter > 0, thus do nothing
    jmp txdevatt ; do the work
prdevatt__nop = *
    rts ; do nothing

    ; txdevatt(): Transmit "^[[?6c" (VT102) in response to the
    ; device attributes (DA) and DECID escape sequences.
txdevatt = *
    ; Set up a pointer to a null terminated string for txstr() to transmit
    lda #txdevatt__str&255
    sta addr0
    lda #txdevatt__str>>8
    sta addr0+1
    ; Use txstr() to transmit the string, then return
    jmp txstr
txdevatt__str = *
    db 27
    asc "[?6c"
    db 0

    ; prstatus(): Handle the "^[[...n" escape sequences:
    ;       ^[[5n - terminal status; respond with ^[[0n which means ok
    ;       ^[[6n - cursor position; respond with ^[[row;colR
prstatus = *
    lda numbufh
    bne prstatus__nop ; parameter >= 256, thus do nothing
    lda numbufl
    cmp #5
    beq prstatus__dsr ; parameter == 5, device status report (DSR)
    cmp #6
    beq prstatus__cpr ; parameter == 6, cursor position report (CPR)
prstatus__nop = *
    rts ; any unknown value for 1st parameter: ignore
prstatus__dsr = *
    ; The sequence ^[[5n requests a device status report.  The response is
    ; ^[[0n if the device is not ok.  I'll consider any circumstance
    ; where the device is receiving and responding to these escape
    ; sequences as "OK".  (VTJ-1 doesn't have a lot of hardware
    ; diagnostics; the only one so far really is the keyboard.)
    lda #prstatus__dsrstr&255
    sta addr0
    lda #prstatus__dsrstr>>8
    sta addr0+1
    jmp txstr ; transmit string with txstr(), then return
prstatus__dsrstr = *
    db 27
    asc "[0n"
    db 0
prstatus__cpr = *
    ; The sequence ^[[6n requests a cursor position report in the form
    ;   ^[[row;colR
    lda #27 ; transmit escape
    jsr txchar
    lda #'[' ; transmit bracket
    jsr txchar
    ldx cursor_row ; transmit the cursor row in decimal
    inx
    txa
    jsr txdecimal
    lda #';' ; transmit delimiter of parameters
    jsr txchar
    ldx cursor_col ; transmit the cursor column in decimal
    inx
    txa
    jsr txdecimal
    lda #'R' ; transmit ending that marks this as position report, & return
    jmp txchar

    ; prleds() - Set programmable LEDs using the ^[[...q escape sequence.
    ; The first parameter (the only one looked at) says what to do:
    ;       0 - clear all LEDs
    ;       1-3 - set LEDs on keyboard (scroll, num, and caps lock)
    ;       4- - set onboard LEDs if any
prleds = *
    lda numbufh ; upper half of 1st parameter
    bne prleds__ret ; parameter >= 256: out of range, ignore
    lda numbufl ; lower half of 1st parameter
    beq prleds__zero ; parameter 0 or missing: clear all LEDs
    cmp #4
    bpl prleds__onboard ; parameter >=4: onboard LEDs through GPIO
    ; parameter 1-3: Keyboard LEDs set over the PS/2 bus
    tax ; look up value identifying this LED
    lda powers_tbl-1,x
    ora keyled_state
    sta keyled_state
    jmp keyled_update ; send the new value to the keyboard; and return
prleds__onboard = *
    ; parameter >=4: Set onboard LEDs via GPIO.  May support up to 8 but
    ; of course not all boards have that many.
    sec ; subtract 4 to get the onboard LED number
    sbc #4
    cmp #8 ; see if it's too high
    bpl prleds__ret ; if so, do nothing
    tax ; look up in a table of exponents, to find this LED's bit value
    lda powers_tbl,x
    ora LEDOUT ; combine with other LEDs that are already set
    sta LEDOUT
prleds__ret = *
    rts ; done
prleds__zero = *
    ; parameter 0 or missing: Clear all LEDs
    ; note that at this point, Accumulator is zero
    sta LEDOUT ; turn off onboard LEDs
    sta keyled_state ; turn off keyboard LEDs
    jmp keyled_update ; do that keyboard LED update, and return

;; ;; ;; ;; Code: on-screen menu

IF ENABLE_MENU
    ; menu_mode() puts the terminal in menu mode, triggered by key
    ; combinations alt-apps and ctrl-alt-delete.  In menu mode, the terminal
    ; interacts with the user and not the computer.  The user can change
    ; various terminal settings.  menu_mode() sets up menu mode, then goes
    ; to menu_loop().
menu_mode = *
    ; Issue XOFF where enabled
    lda modeflags2 ; see if it's enabled
    and #MODEFLAGS2_XONXOFF
    beq menu_mode__noxoff
    lda #XOFF_CHAR ; it's enabled, send it
    jsr txchar
    lda #3 ; and record the fact that we've done so
    sta xofflevel
menu_mode__noxoff = *
    ; Initialize menu state
    lda #0 ; start without any field selected
    sta menu_fld_ptr
    sta menu_fld_ptr+1
    ; Clear the screen & display the menu header.
    jsr prstring
    db 65 ; junk character just to consume some modifying state
    db 24 ; cancel any pending escape sequence
    db 27 ; turn off insert mode, it makes things look wrong
    asc "[4l"
    db 27 ; turn off keyboard lock mode, not because menu mode cares about
          ; it, but because it's sometimes annoying to the user
    asc "[?8l"
    db 27 ; set scroll range to whole screen
    asc "[r"
    db 27 ; set character attributes to default
    asc "[m"
    db 27 ; set character set to default
    asc "[(B"
    db 15
    db 27 ; move cursor to top
    asc "[H"
    db 27 ; clear screen
    asc "[2J"
    asc "VTJ-1 Menu"
    db 27 ; double height, upper half
    asc "#3"
    db 1 ; narrow
    db 13, 10
    asc "VTJ-1 Menu"
    db 27 ; double height, lower half
    asc "#4"
    db 1 ; narrow
    db 13, 10
    asc "Keys: <Esc> exits, -> next item, <Space> performs action"
    db 0 ; end of the 'jsr prstring'
    lda #0 ; And clear the modifier keys state, since we don't in menu_loop()
           ; and the user will probably release the modifier keys before
           ; getting out of menu mode.
    sta keymod_state
    ; Display the menu
    jsr menu_display
    ; And let the user interact with it
    jmp menu_loop

    ; menu_loop() is an event handling loop, similar to main_loop(), but
    ; instead of interacting with the host computer it just presents the
    ; terminal's own menu
menu_loop = *
    jsr getinput ; get an input event (type in A, byte value in X)
    cmp #INBUF_ET_PS2CH ; is it a keyboard event?
    bne menu_loop ; if not, ignore
    txa ; it's a keyboard event; get the key code into A
    cmp #$F0
    bne menu_loop__notbrkpfx
    ; $F0 from keyboard indicates break code coming up
    lda keycoding_state
    ora #$80
    sta keycoding_state
    jmp menu_loop
menu_loop__notbrkpfx = *
    cmp #$E0
    bne menu_loop__notextpfx
    ; $E0 from keyboard indicates extended code coming up
    lda keycoding_state
    ora #$01
    sta keycoding_state
    jmp menu_loop
menu_loop__notextpfx = *
    ; This must be the keycode for a key, possibly with modifiers
    ; (PS/2 protocol modifier bytes; not modifier keys, because menu
    ; mode doesn't recognize them)
    lda keycoding_state ; grab the modifier state
    ldy #0 ; clear the state
    sty keycoding_state
    tay ; test the sign ($80 bit) of the modifier state
    bmi menu_loop ; it's a "break" code; ignore it
    and #1 ; test the $01 bit of the modifier state
    beq menu_loop__not_ext
    ; this is an "extended" code & we look it up by setting the high bit
    txa
    ora #$80
    tax
menu_loop__not_ext = *
    ; look up the modified key code byte in keycode_tbl
    lda keycode_tbl,x
    ; most of the codes in that table are ignored in menu mode; only
    ; the following are recognized:
    ;   arrows ($91 down, $92 right)
    ;   space ($20) or enter ($A0)
    ;   escape ($1B)
    cmp #$20 ; is it space?
    bne menu_loop__not_spc
    jmp menu_key_enter
menu_loop__not_spc = *
    cmp #$A0 ; is it enter?
    bne menu_loop__not_enter
    jmp menu_key_enter
menu_loop__not_enter = *
    cmp #$91 ; is it a down arrow?
    bne menu_loop__not_down
    jsr menu_key_next_line ; go to next line
    jsr menu_display ; redraw the menu to show it
    jmp menu_loop ; and wait for more keystrokes
menu_loop__not_down = *
    cmp #$92 ; is it a right arrow?
    bne menu_loop__not_right
    jsr menu_next_field ; go to next field
    jsr menu_display ; redraw the menu to show it
    jmp menu_loop ; and wait for more keystrokes
menu_loop__not_right = *
    cmp #$1B ; is it escape?
    bne menu_loop__not_esc
    jmp menu_key_escape
menu_loop__not_esc = *
    ; It isn't anything recognized; just wait for another key.
    jmp menu_loop

    ; menu_key_enter(): In menu mode, have received an enter or space
    ; key, which results in an action.  Instead of returning this jumps
    ; back to menu_loop().
menu_key_enter = *
    ; menu_fld_ptr points to the currently selected field.  Is it NULL?
    lda menu_fld_ptr
    ora menu_fld_ptr+1
    beq menu_key_enter__done ; if it's NULL, do nothing: no field selected
    ; so it points to the $03 byte which starts a field
    ldy #3 ; get the set callback code pointer into atmp so we can call it
    lda (menu_fld_ptr),y
    sta atmp
    iny
    lda (menu_fld_ptr),y
    sta atmp+1
    iny ; and get the parameter into the accumulator
    lda (menu_fld_ptr),y
    jsr atmp_indirector ; and run that callback, which will perform the
                        ; action of this menu field
    jsr menu_display ; redisplay the whole menu as a result
menu_key_enter__done = *
    jmp menu_loop ; done

    ; menu_key_next_line(): In menu mode, have received a down-arrow key,
    ; which moves the selection to the next line, and the
    ; leftmost field on it.
menu_key_next_line = *
    lda #0 ; use 'menu_saw_lf' to see when we get to another line
    sta menu_saw_lf
    jsr menu_next_field ; next field
    lda menu_saw_lf ; did we go to a new line?
    beq menu_key_next_line ; no: keep going through the fields
    rts ; yes: done

    ; menu_next_field(): In menu mode, have received a right-arrow key,
    ; which moves the selection to the next field.
    ; Also, whenever it sees an LF character, it sets menu_saw_lf.
menu_next_field = *
    lda menu_fld_ptr ; is menu_fld_ptr == NULL?
    ora menu_fld_ptr+1
    bne menu_next_field__notnull
menu_next_field__null = *
    ; either:
    ;       menu_fld_ptr == NULL, meaning no field is selected
    ;       or *menu_fld_ptr == '\0', meaning we got to the end of the menu
    ; either way, go to start of the menu
    lda #menu_data&255
    sta menu_fld_ptr
    lda #menu_data>>8
    sta menu_fld_ptr+1
    sta menu_saw_lf ; indicate that we got to a new line
                    ; (assumes: menu_data is not in zero page, which
                    ; seems a very reasonable assumption)
    jmp menu_next_field__skiptext
menu_next_field__notnull = *
    ; It's not null.  Presumably it's pointing to a byte value $03, starting
    ; a 6 byte field descriptor.  Skip over it, and any text until the next
    ; one.
    lda #6
    clc
    adc menu_fld_ptr
    sta menu_fld_ptr
    lda #0
    adc menu_fld_ptr+1
    sta menu_fld_ptr+1
menu_next_field__skiptext = *
    ; Skip text byte by byte until we get to the next field or run out
    ; of text.
    ldy #0 ; what is the byte it's pointing to?
    lda (menu_fld_ptr),y
    beq menu_next_field__null ; byte 0: end of menu, go to start
    cmp #3 ; see about byte 3, indicating a new menu field
    bne menu_next_field__notthree
    rts ; return
menu_next_field__notthree = *
    cmp #10 ; see about byte 10, line feed (new line)
    bne menu_next_field__notnl
    sta menu_saw_lf ; indicate that we got to a new line
menu_next_field__notnl = *
    ; not interested, just skip the byte
menu_next_field__next = *
    ; on to the next byte
    inc menu_fld_ptr
    bne menu_next_field__skiptext
    inc menu_fld_ptr+1
    bne menu_next_field__skiptext ; branch always taken unless something
                                  ; horrible has happened

    ; menu_key_escape(): In menu mode, have received an escape key,
    ; which results in returning to terminal mode.
menu_key_escape = *
    ; send XOFF (if enabled)
    jsr send_xon_xoff ; send XOFF (if enabled)
    ; clear screen
    jsr prstring
    db 27 ; set character attributes to default
    asc "[m"
    db 27 ; move cursor to top
    asc "[H"
    db 27 ; clear screen
    asc "[2J"
    db 0 ; end of the 'jsr prstring'
    rts ; return from menu_mode()

    ; menu_display()
    ; Display the menu that's in 'menu_data' encoded as described there.
    ; Currently enabled fields will be prefixed with '*' (vs ' ').
    ; The currently selected field will be in inverse video.
menu_display = *
    jsr prstring ; go to row 4 of the screen
    db 27
    asc "[4H"
    db 0
    lda #menu_data&255 ; start at the start of menu_data, with btmp as pointer
    sta btmp
    lda #menu_data>>8
    sta btmp+1
menu_display__loop = *
    ; read one byte from menu_data & do what it tells us to
    ldy #0 ; read the byte pointed to by btmp
    lda (btmp),y
    beq menu_display__end ; byte 0: end of menu data
    cmp #3
    beq menu_display__sf ; byte 3: start selectable field
    cmp #4
    beq menu_display__ef ; byte 4: end selectable field
    ; other bytes: display
    jsr prbyte
menu_display__next = *
    ; go on to the next byte
    inc btmp
    bne menu_display__loop
    inc btmp+1
    bne menu_display__loop ; always true or something horrible has happened
menu_display__end = *
    ; byte 0: end of menu data; that's all there is to do
    rts
menu_display__ef = *
    ; byte 4: end selectable field; turn off inverse video in case it 
    ; was turned on
    jsr prstring
    db 27
    asc "[27m"
    db 0
    jmp menu_display__next
menu_display__sf = *
    ; byte 3: start selectable field; there's a lot to do here:
    ;       + is the field selected? if so, go to inverse video
    ;       + is the field enabled? prefix with a '*', otherwise ' '
    ;       + skip over this 6 byte metasymbol
    lda menu_fld_ptr ; is it selected? check 'menu_fld_ptr == btmp'
    cmp btmp
    bne menu_display__sf_nonsel
    lda menu_fld_ptr+1
    cmp btmp+1
    bne menu_display__sf_nonsel
    ; this field is selected; highlight it with inverse video
    jsr prstring
    db 27
    asc "[7m"
    db 0
menu_display__sf_nonsel = *
    ; whether the field is selected or not, we should show if it's enabled
    ldy #1 ; read the code pointer from the menu data into atmp
    lda (btmp),y
    sta atmp
    iny
    lda (btmp),y
    sta atmp+1
    ldy #5 ; and read the parameter into accumulator
    lda (btmp),y
    jsr atmp_indirector ; and figure out a char to tell if field is enabled
    jsr prbyte ; and display that character
    lda btmp ; now advance btmp by 6 bytes
    clc
    adc #6
    sta btmp
    lda btmp+1
    adc #0
    sta btmp+1
    jmp menu_display__loop ; and do the next byte

    ; menu_get_baud()
    ; Determine if the current baud rate control byte equals the value
    ; in the accumulator.  Returns 1 character indicating the result.
menu_get_baud = *
    cmp SER_BASE+SER_RA
    beq menu_get_baud__ena
    lda #' ' ; disabled
    rts
menu_get_baud__ena = *
    lda #'*' ; enabled
    rts

    ; menu_set_baud()
    ; Set the baud rate to the control byte value given in the accumulator.
menu_set_baud = *
    sta SER_BASE+SER_RA
    sta SER_BASE+SER_TA
    rts

    ; menu_modeflags_util()
    ; This function is used by mode_get_modeflags() & mode_set_modeflags()
    ; to parse the various fields they get in the accumulator.
    ; Input accumulator contents:
    ;   add $80 for a '1' bit value; otherwise match '0' bit value
    ;   add $08 to match 'modeflags2'; otherwise match 'modeflags1'
    ;   add $00-$07 to select which bit (by logarithm)
    ; Output:
    ;   Y holds the address of 'modeflags1' or 'modeflags2', whichever it is
    ;   accumulator holds the bit mask value ($01, $02, etc)
    ;   carry flag set with the bit value to use/match
menu_modeflags_util = *
    tax ; save for later
    and #$08 ; check, which mode flags byte to use
    beq menu_modeflags_util__byte1
    ; using modeflags2
    ldy #modeflags2 ; intentionally using immediate mode to store an
                    ; address into Y
    jmp menu_modeflags_util__bytedone
menu_modeflags_util__byte1 = *
    ; using modeflags1
    ldy #modeflags1 ; intentionally using immediate mode to store an
                    ; address into Y
menu_modeflags_util__bytedone = *
    txa ; retrieve the accumulator value again
    rol a ; get the top bit into the carry flag
    txa ; retrieve the accumulator value again
    and #$07 ; get the bit position index
    tax ; and use it to look up the bit value
    lda powers_tbl,x
    rts

    ; menu_get_modeflags()
    ; Check one mode flag bit; the bit field is indicated in
    ; the accumulator.  Returns 1 character indicating the result.
    ; Accumulator contents: see menu_modeflags_util()
menu_get_modeflags = *
    jsr menu_modeflags_util ; process the accumulator byte contents
    and 0,y ; test the flag bit that's in question
    beq menu_get_modeflags__z ; branches if the flag bit is zero
    bcc menu_get_modeflags__unmatched ; branches if matching zero flag bit
menu_get_modeflags__matched = *
    ; Either the flag bit is zero & that's what we're matching, or it's
    ; one and that's what we're matching: It matches so use the '*'
    ; character
    lda #'*'
    rts
menu_get_modeflags__z = *
    bcc menu_get_modeflags__matched ; branches if matching zero flag bit
menu_get_modeflags__unmatched = *
    ; Either the flag bit is zero & we're matching one, or it's one and
    ; we're matching zero:  It doesn't match so use the ' ' character.
    lda #' '
    rts

    ; menu_set_modeflags()
    ; Set/clear one mode flag bit; the bit field and the operation are
    ; indicated in the accumulator.
    ; Accumulator contents: see menu_modeflags_util()
menu_set_modeflags = *
    jsr menu_modeflags_util ; process the accumulator byte contents
    bcc menu_set_modeflags__clearit ; branch if we're clearing the bit
    ; we're setting the bit
    ora 0,y ; combine that bit with the others in the same byte
    sta 0,y
    rts
menu_set_modeflags__clearit = *
    ; we're clearing the bit
    eor #$ff
    and 0,y
    sta 0,y
    rts
ENDC ; IF ENABLE_MENU

;; ;; ;; ;; Lookup tables

pgalign

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

    dw serial_rx_irq     ; slot 8 alpha interrupt (serial port, RX possible)
    dw unimplemented_irq ; slot 9 alpha interrupt (empty slot)
    dw unimplemented_irq ; slot 10 alpha interrupt (empty slot)
    dw unimplemented_irq ; slot 11 alpha interrupt (empty slot)
    dw ps2_a_rx_irq      ; slot 12 alpha interrupt (PS/2 port A, RX possible)
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

prcontrol_tbl = *
    ; Dispatch table for control characters in prbyte() in normal mode.
    ; 32 entries.  They'll be called with the character code in the
    ; accumulator.
    ; YYY fill in more of these
    dw prcontrol_unk ; byte 0: NUL - caught upstream & ignored: never gets here
    dw prcontrol_unk ; byte 1: SOH - unsupported in VTJ-1
    dw prcontrol_unk ; byte 2: STX - unsupported in VTJ-1
    dw prcontrol_unk ; byte 3: ETX - unsupported in VTJ-1
    dw prcontrol_unk ; byte 4: EOT - unsupported in VTJ-1
    dw prnop         ; byte 5: ENQ - enquiry; here, a no-op
    dw prcontrol_unk ; byte 6: ACK - unsupported in VTJ-1
    dw prbell        ; byte 7: BEL - visible or audible bell
    dw prbackspace   ; byte 8: BS, backspace
    dw prtab         ; byte 9: HT, horizontal tab
    dw prlf          ; byte 10: LF, also known as newline
    dw prlf          ; byte 11: VT; VT102 treats it like LF so so do we
    dw prlf          ; byte 12: FF; VT102 treats it like LF so so do we
    dw prcr          ; byte 13: CR, also known as return
    dw prcontrol_cset; byte 14: SO, select G1 character set
    dw prcontrol_cset; byte 15: S1, select G0 character set
    dw prcontrol_unk ; byte 16: SO - unsupported in VTJ-1
    dw prcontrol_unk ; byte 17: DC1 - XON; not yet implemented in RX side
    dw prcontrol_unk ; byte 18: DC2 - unsupported in VTJ-1
    dw prcontrol_unk ; byte 19: DC3 - XOFF; not yet implemented in RX side
    dw prcontrol_unk ; byte 20: DC4 - unsupported in VTJ-1
    dw prcontrol_unk ; byte 21: NAK - unsupported in VTJ-1
    dw prcontrol_unk ; byte 22: unknown
    dw prcontrol_unk ; byte 23: ETB - unsupported in VTJ-1
    dw prcontrol_unk ; byte 24: CAN - handled specially, upstream
    dw prcontrol_unk ; byte 25: EM - unsupported in VTJ-1
    dw prcontrol_unk ; byte 26: SUB - handled specially, upstream
    dw prcontrol_esc ; byte 27: ESC - escape character
    dw prcontrol_unk ; byte 28: FS - unsupported in VTJ-1
    dw prcontrol_unk ; byte 29: GS - unsupported in VTJ-1
    dw prcontrol_unk ; byte 30: RS - unsupported in VTJ-1
    dw prcontrol_unk ; byte 31: US - unsupported in VTJ-1

prescps_tbl = *
    ; Dispatch table for "ESC #" sequences, of which there are at most 10.
    dw prcontrol_unk   ; ESC # 0 (unknown)
    dw prcontrol_unk   ; ESC # 1 (unknown)
    dw prcontrol_unk   ; ESC # 2 (unknown)
    dw prescps_rowdhu  ; ESC # 3 (row: double width double height upper half)
    dw prescps_rowdhl  ; ESC # 4 (row: double width double height lower half)
    dw prescps_rowswh  ; ESC # 5 (row: single width single height)
    dw prescps_rowdw   ; ESC # 6 (row: double width single height)
    dw prcontrol_unk   ; ESC # 7 (unknown)
    dw prescps_eeeee   ; ESC # 8 (fill screen with upper case 'E's, really)
    dw prcontrol_unk   ; ESC # 9 (unknown)

prstate_tbl = *
    ; Dispatch table for the different states for prbyte().  States are
    ; even-numbered for easy lookup
prstate_ety macro
    \1 = * - prstate_tbl
    dw \2
endm
    prstate_ety PRSTATE_NORMAL, prbyte_normal ; print what we get, mostly
    prstate_ety PRSTATE_ESC, prbyte_esc ; have read ESC
    prstate_ety PRSTATE_ESCLB, prbyte_esclb ; read ESC [ and more
    prstate_ety PRSTATE_ESCPS, prbyte_escps ; read ESC #
    prstate_ety PRSTATE_ESCLB1, prbyte_esclb1 ; read ESC [ nothing more
    prstate_ety PRSTATE_ESCLBI, prbyte_esclbi ; in ESC ['s "intermediate" bytes
    prstate_ety PRSTATE_ESCLPAR, prbyte_esclpar ; have read ESC (
    prstate_ety PRSTATE_ESCRPAR, prbyte_escrpar ; have read ESC )

pgalign

    ; Lookup tables for fifty different numeric codes used in
    ; ESC [ ... m.  A pair of code pointer table and a parameter table,
    ; the meaning of which depends on the code.
prattcode_jmp_tbl = *
    dw prattcode_default ; code 0 - back to default
    dw prattcode_setbit  ; code 1 - bold (brighten foreground)
    dw prattcode_nop
    dw prattcode_nop
    dw prattcode_setbit  ; code 4 - underline
    dw prattcode_nop
    dw prattcode_nop
    dw prattcode_inverse ; code 7 - inverse video
    dw prattcode_nop
    dw prattcode_nop
    dw prattcode_nop     ; code 10
    dw prattcode_nop
    dw prattcode_nop
    dw prattcode_nop
    dw prattcode_nop
    dw prattcode_nop     ; code 15
    dw prattcode_nop
    dw prattcode_nop
    dw prattcode_nop
    dw prattcode_nop
    dw prattcode_nop     ; code 20
    dw prattcode_nop
    dw prattcode_clrbit  ; code 22 - no bold
    dw prattcode_nop
    dw prattcode_clrbit  ; code 24 - no underline
    dw prattcode_nop
    dw prattcode_nop
    dw prattcode_inverse ; code 27 - no inverse
    dw prattcode_nop
    dw prattcode_nop
    dw prattcode_color   ; code 30 - black foreground
    dw prattcode_color   ; code 31 - red foreground
    dw prattcode_color   ; code 32 - green foreground
    dw prattcode_color   ; code 33 - yellow foreground
    dw prattcode_color   ; code 34 - blue foreground
    dw prattcode_color   ; code 35 - magenta foreground
    dw prattcode_color   ; code 36 - cyan foreground
    dw prattcode_color   ; code 37 - white foreground
    dw prattcode_nop
    dw prattcode_color   ; code 39 - default foreground
    dw prattcode_color   ; code 40 - black background
    dw prattcode_color   ; code 41 - red background
    dw prattcode_color   ; code 42 - green background
    dw prattcode_color   ; code 43 - yellow background
    dw prattcode_color   ; code 44 - blue background
    dw prattcode_color   ; code 45 - magenta background
    dw prattcode_color   ; code 46 - cyan background
    dw prattcode_color   ; code 47 - white background
    dw prattcode_nop
    dw prattcode_color   ; code 49 - default background

prattcode_arg_tbl = *
    db $00 ; code 0 - back to default
    db $08 ; code 1 - bold (brighten foreground)
    db $00, $00 ; codes 2-3 - nothing
    db $80 ; code 4 - underline
    db $00, $00 ; codes 5-6 - nothing
    db $01 ; code 7 - inverse video
    db $00, $00, $00, $00, $00, $00, $00 ; codes 8-14 - nothing
    db $00, $00, $00, $00, $00, $00, $00 ; codes 15-21 - nothing
    db $08 ; code 22 - no bold
    db $00 ; code 23 - nothing
    db $80 ; code 24 - no underline
    db $00, $00 ; codes 25-26 - nothing
    db $00 ; code 27 - no inverse video
    db $00, $00 ; codes 28-29 - nothing
    db $00, $01, $02, $03, $04, $05, $06, $07 ; codes 30-37 - foreground colors
    db $00 ; nothing
    db DEFFG ; code 39 - default foreground color
    db $80, $81, $82, $83, $84, $85, $86, $87 ; codes 40-47 - background colors
    db $00 ; nothing
    db $80 ; code 49 - default background color, black

    ; prmode_lst_* - 
    ; list of mode flags, they can be set and reset through "ESC [ ... h"
    ; and "ESC [ ... l" respectively.  There are seven "byte planes" in the
    ; list, the entries of which correspond to one another, make sure not
    ; to get them mismatched.  The flags are listed in order of increasing
    ; value, but there's no technical requirement for that; it's just
    ; for easier maintenance.

prmode_lst_lbvar = * ; esc_lbvar values to match, end with ESC_LBVAR_INVALID
    db ESC_LBVAR_EQUAL ; ESC [ = 1 {l,h}
    db ESC_LBVAR_QUES  ; ESC [ ? 1 {l,h}
    db ESC_LBVAR_NORMAL; ESC [   2 {l,h}
    db ESC_LBVAR_EQUAL ; ESC [ = 3 {l,h}
    db ESC_LBVAR_NORMAL; ESC [   4 {l,h}
    db ESC_LBVAR_EQUAL ; ESC [ = 4 {l,h}
    db ESC_LBVAR_EQUAL ; ESC [ = 5 {l,h}
    db ESC_LBVAR_QUES  ; ESC [ ? 5 {l,h}
    db ESC_LBVAR_QUES  ; ESC [ ? 6 {l,h}
    db ESC_LBVAR_QUES  ; ESC [ ? 7 {l,h}
    db ESC_LBVAR_QUES  ; ESC [ ? 8 {l,h}
    db ESC_LBVAR_NORMAL; ESC [   12 {l,h}
    db ESC_LBVAR_NORMAL; ESC [   20 {l,h}

    ; and one extra entry on prmode_lst_lbvar, not found on the other
    ; prmode_lst_*: ESC_LBVAR_INVALID to mark the end
    db ESC_LBVAR_INVALID ; that's all

prmode_lst_value = * ; numeric parameter value 0-255
    db 1 ; ESC [ = 1 {l,h}
    db 1 ; ESC [ ? 1 {l,h}
    db 2 ; ESC [   2 {l,h}
    db 3 ; ESC [ = 3 {l,h}
    db 4 ; ESC [   4 {l,h}
    db 4 ; ESC [ = 4 {l,h}
    db 5 ; ESC [ = 5 {l,h}
    db 5 ; ESC [ ? 5 {l,h}
    db 6 ; ESC [ ? 6 {l,h}
    db 7 ; ESC [ ? 7 {l,h}
    db 8 ; ESC [ ? 8 {l,h}
    db 12; ESC [   12 {l,h}
    db 20; ESC [   20 {l,h}

prmode_lst_set0 = * ; low halves of pointers to "set" code
    db modeflags1_set&255 ; ESC [ = 1 {l,h}
    db modeflags1_set&255 ; ESC [ ? 1 {l,h}
    db modeflags2_set&255 ; ESC [   2 {l,h}
    db modeflags1_set&255 ; ESC [ = 3 {l,h}
    db modeflags2_set&255 ; ESC [   4 {l,h}
    db modeflags1_set&255 ; ESC [ = 4 {l,h}
    db modeflags1_set&255 ; ESC [ = 5 {l,h}
    db invert_screen&255  ; ESC [ ? 5 {l,h}
    db origin_mode_set&255; ESC [ ? 6 {l,h}
    db modeflags2_set&255 ; ESC [ ? 7 {l,h}
    db key_repeat_on&255  ; ESC [ ? 8 {l,h}
    db modeflags2_set&255 ; ESC [ 12 {l,h}
    db modeflags2_set&255 ; ESC [ 20 {l,h}

prmode_lst_set1 = * ; high halves of pointers to "set" code
    db modeflags1_set>>8 ; ESC [ = 1 {l,h}
    db modeflags1_set>>8 ; ESC [ ? 1 {l,h}
    db modeflags2_set>>8 ; ESC [   2 {l,h}
    db modeflags1_set>>8 ; ESC [ = 3 {l,h}
    db modeflags2_set>>8 ; ESC [   4 {l,h}
    db modeflags1_set>>8 ; ESC [ = 4 {l,h}
    db modeflags1_set>>8 ; ESC [ = 5 {l,h}
    db invert_screen>>8  ; ESC [ ? 5 {l,h}
    db origin_mode_set>>8; ESC [ ? 6 {l,h}
    db modeflags2_set>>8 ; ESC [ ? 7 {l,h}
    db key_repeat_on>>8  ; ESC [ ? 8 {l,h}
    db modeflags2_set>>8 ; ESC [ 12 {l,h}
    db modeflags2_set>>8 ; ESC [ 20 {l,h}

prmode_lst_reset0 = * ; low halves of pointers to "reset" code
    db modeflags1_reset&255 ; ESC [ = 1 {l,h}
    db modeflags1_reset&255 ; ESC [ ? 1 {l,h}
    db modeflags2_reset&255 ; ESC [   2 {l,h}
    db modeflags1_reset&255 ; ESC [ = 3 {l,h}
    db modeflags2_reset&255 ; ESC [   4 {l,h}
    db modeflags1_reset&255 ; ESC [ = 4 {l,h}
    db modeflags1_reset&255 ; ESC [ = 5 {l,h}
    db uninvert_screen&255  ; ESC [ ? 5 {l,h}
    db origin_mode_reset&255; ESC [ ? 6 {l,h}
    db modeflags2_reset&255 ; ESC [ ? 7 {l,h}
    db key_repeat_off&255   ; ESC [ ? 8 {l,h}
    db modeflags2_reset&255 ; ESC [ 12 {l,h}
    db modeflags2_reset&255 ; ESC [ 20 {l,h}

prmode_lst_reset1 = * ; high halves of pointers to "reset" code
    db modeflags1_reset>>8 ; ESC [ = 1 {l,h}
    db modeflags1_reset>>8 ; ESC [ ? 1 {l,h}
    db modeflags2_reset>>8 ; ESC [   2 {l,h}
    db modeflags1_reset>>8 ; ESC [ = 3 {l,h}
    db modeflags2_reset>>8 ; ESC [   4 {l,h}
    db modeflags1_reset>>8 ; ESC [ = 4 {l,h}
    db modeflags1_reset>>8 ; ESC [ = 5 {l,h}
    db uninvert_screen>>8  ; ESC [ ? 5 {l,h}
    db origin_mode_reset>>8; ESC [ ? 6 {l,h}
    db modeflags2_reset>>8 ; ESC [ ? 7 {l,h}
    db key_repeat_off>>8   ; ESC [ ? 8 {l,h}
    db modeflags2_reset>>8 ; ESC [ 12 {l,h}
    db modeflags2_reset>>8 ; ESC [ 20 {l,h}

prmode_lst_acc = * ; byte to have in accumulator for set/reset code
    db MODEFLAGS1_CHECKERBOARD ; ESC [ = 1 {l,h}
    db MODEFLAGS1_CURSORKEY    ; ESC [ ? 1 {l,h}
    db MODEFLAGS2_LOCKKB       ; ESC [   2 {l,h}
    db MODEFLAGS1_BLOCKCURSOR  ; ESC [ = 3 {l,h}
    db MODEFLAGS2_INSERT       ; ESC [   4 {l,h}
    db MODEFLAGS1_VISBELL      ; ESC [ = 4 {l,h}
    db MODEFLAGS1_AUDBELL      ; ESC [ = 5 {l,h}
    db 0 ; this byte not used  ; ESC [ ? 5 {l,h}
    db 0 ; this byte not used  ; ESC [ ? 6 {l,h}
    db MODEFLAGS2_WRAP         ; ESC [ ? 7 {l,h}
    db 0 ; this byte not used  ; ESC [ ? 8 {l,h}
    db MODEFLAGS2_NOECHO       ; ESC [ 12 {l,h}
    db MODEFLAGS2_LFNL         ; ESC [ 20 {l,h}

    ; cset_lst_* - List of character code identifying characters, comprising
    ; two lists with byte entries corresponding to one another:
    ;   cset_lst_char - the characters
    ;   cset_lst_code - the corresponding values to put in 'cset'
    ; the end is marked by adding a character with the high bit set
cset_lst_char = *
    db 'A', 'B', '0', '1', '2', 255
cset_lst_code = *
    db 1, 0, 64, 128, 192

pgalign

    ; keycode_tbl: Lookup table used during processing of keyboard
    ; scan codes from the PS/2 port.  This table is the center of
    ; the processing, but it's neither the first nor last word of it.
    ; Before lookup in the table, the scancode is combined into a single
    ; byte as follows:
    ;   + A single byte scancode, $00-$83, is looked up as is
    ;   + A double byte scancode, $E0 $04-$7F, is looked up by the second
    ;   byte plus $80
    ;   + Anything else can't use the table.
    ; And the value in the table is also encoded, as follows:
    ;   + If the key corresponds to an ASCII character, then that character's
    ;   value is given in the table (for letters, the lower case is given)
    ;   + If not, it's encoded as some other byte value:
    ;       $00 - unrecognized or unimplemented scan code
    ;       $80-$87 - non sticky modifier keys
    ;           $80 - shift
    ;           $81 - control
    ;           $82 - alt
    ;       $88-$8f - sticky modifier keys
    ;           $88 - caps lock
    ;           $89 - num lock
    ;           $8a - scroll lock
    ;       $90-$93 - cursor movement arrow keys
    ;           $90 - up
    ;           $91 - down
    ;           $92 - right
    ;           $93 - left
    ;       $a0 - miscellaneous
    ;           $a0 - ENTER/RETURN key: emits CR or CR+LF depending on mode
    ;           $a1 - APPS key
    ;       $b0-$b5 - editing keys
    ;           $b0 - insert
    ;           $b1 - home
    ;           $b2 - page down
    ;           $b3 - delete
    ;           $b4 - end
    ;           $b5 - page down
    ; This table is derived from scan code set 2 (the default used by keyboards
    ; nowadays) as described in:
    ;   http://www.computer-engineering.org/ps2keyboard/scancodes2.html
keycode_tbl = *
    ; YYY fill in all these that are applicable, eventually
    db $00 ; 00
    db $00 ; 01 "F9"
    db $00 ; 02
    db $00 ; 03 "F5"
    db $00 ; 04 "F3"
    db $00 ; 05 "F1"
    db $00 ; 06 "F2"
    db $00 ; 07 "F12"
    db $00 ; 08
    db $00 ; 09 "F10"
    db $00 ; 0A "F8"
    db $00 ; 0B "F6"
    db $00 ; 0C "F4"
    db $09 ; 0D "TAB"
    db $60 ; 0E "`"
    db $00 ; 0F
    db $00 ; 10
    db $82 ; 11 "L ALT"
    db $80 ; 12 "L SHFT"
    db $00 ; 13
    db $81 ; 14 "L CTRL"
    db $71 ; 15 "Q"
    db $31 ; 16 "1"
    db $00 ; 17
    db $00 ; 18
    db $00 ; 19
    db $7A ; 1A "Z"
    db $73 ; 1B "S"
    db $61 ; 1C "A"
    db $77 ; 1D "W"
    db $32 ; 1E "2"
    db $00 ; 1F
    db $00 ; 20
    db $63 ; 21 "C"
    db $78 ; 22 "X"
    db $64 ; 23 "D"
    db $65 ; 24 "E"
    db $34 ; 25 "4"
    db $33 ; 26 "3"
    db $00 ; 27
    db $00 ; 28
    db $20 ; 29 "SPACE"
    db $76 ; 2A "V"
    db $66 ; 2B "F"
    db $74 ; 2C "T"
    db $72 ; 2D "R"
    db $35 ; 2E "5"
    db $00 ; 2F
    db $00 ; 30
    db $6E ; 31 "N"
    db $62 ; 32 "B"
    db $68 ; 33 "H"
    db $67 ; 34 "G"
    db $79 ; 35 "Y"
    db $36 ; 36 "6"
    db $00 ; 37
    db $00 ; 38
    db $00 ; 39
    db $6D ; 3A "M"
    db $6A ; 3B "J"
    db $75 ; 3C "U"
    db $37 ; 3D "7"
    db $38 ; 3E "8"
    db $00 ; 3F
    db $00 ; 40
    db $2C ; 41 "COMMA"
    db $6B ; 42 "K"
    db $69 ; 43 "I"
    db $6F ; 44 "O"
    db $30 ; 45 "0"
    db $39 ; 46 "9"
    db $00 ; 47
    db $00 ; 48
    db $2E ; 49 "."
    db $2F ; 4A "/"
    db $6C ; 4B "L"
    db $3B ; 4C ";"
    db $70 ; 4D "P"
    db $2D ; 4E "-"
    db $00 ; 4F
    db $00 ; 50
    db $00 ; 51
    db $27 ; 52 "'"
    db $00 ; 53
    db $5B ; 54 "["
    db $3D ; 55 "="
    db $00 ; 56
    db $00 ; 57
    db $88 ; 58 "CAPS"
    db $80 ; 59 "R SHFT"
    db $A0 ; 5A "ENTER"
    db $5D ; 5B "]"
    db $00 ; 5C
    db $5C ; 5D "\"
    db $00 ; 5E
    db $00 ; 5F
    db $00 ; 60
    db $00 ; 61
    db $00 ; 62
    db $00 ; 63
    db $00 ; 64
    db $00 ; 65
    db $08 ; 66 "BKSP"
    db $00 ; 67
    db $00 ; 68
    db $00 ; 69 "KP 1"
    db $00 ; 6A
    db $00 ; 6B "KP 4"
    db $00 ; 6C "KP 7"
    db $00 ; 6D
    db $00 ; 6E
    db $00 ; 6F
    db $00 ; 70 "KP 0"
    db $00 ; 71 "KP ."
    db $00 ; 72 "KP 2"
    db $00 ; 73 "KP 5"
    db $00 ; 74 "KP 6"
    db $00 ; 75 "KP 8"
    db $1B ; 76 "ESC"
    db $89 ; 77 "NUM"
    db $00 ; 78 "F11"
    db $00 ; 79 "KP +"
    db $00 ; 7A "KP 3"
    db $00 ; 7B "KP -"
    db $00 ; 7C "KP *"
    db $00 ; 7D "KP 9"
    db $8A ; 7E "SCROLL"
    db $00 ; 7F
    db $00 ; 80
    db $00 ; 81
    db $00 ; 82
    db $24 ; 83 "F7" ; YYY this is a temporary value for testing
    db $00 ; E0 04
    db $00 ; E0 05
    db $00 ; E0 06
    db $00 ; E0 07
    db $00 ; E0 08
    db $00 ; E0 09
    db $00 ; E0 0A
    db $00 ; E0 0B
    db $00 ; E0 0C
    db $00 ; E0 0D
    db $00 ; E0 0E
    db $00 ; E0 0F
    db $00 ; E0 10 "WWW Search"
    db $82 ; E0 11 "R ALT"
    db $00 ; E0 12 "PRNT SCRN OUTER"
    db $00 ; E0 13
    db $81 ; E0 14 "R CTRL"
    db $00 ; E0 15 "Previous Track"
    db $00 ; E0 16
    db $00 ; E0 17
    db $00 ; E0 18
    db $00 ; E0 19
    db $00 ; E0 1A
    db $00 ; E0 1B
    db $00 ; E0 1C
    db $00 ; E0 1D
    db $00 ; E0 1E
    db $00 ; E0 1F "L GUI"
    db $00 ; E0 20 "WWW Refresh"
    db $00 ; E0 21 "Volume Down"
    db $00 ; E0 22
    db $00 ; E0 23 "Mute"
    db $00 ; E0 24
    db $00 ; E0 25
    db $00 ; E0 26
    db $00 ; E0 27 "R GUI"
    db $00 ; E0 28 "WWW Stop"
    db $00 ; E0 29
    db $00 ; E0 2A
    db $00 ; E0 2B "Calculator"
    db $00 ; E0 2C
    db $00 ; E0 2D
    db $00 ; E0 2E
    db $a1 ; E0 2F "APPS"
    db $00 ; E0 30 "WWW Forward"
    db $00 ; E0 31
    db $00 ; E0 32 "Volume Up"
    db $00 ; E0 33
    db $00 ; E0 34 "Play/Pause"
    db $00 ; E0 35
    db $00 ; E0 36
    db $00 ; E0 37 "Power"
    db $00 ; E0 38 "WWW Back"
    db $00 ; E0 39
    db $00 ; E0 3A "WWW Home"
    db $00 ; E0 3B "Stop"
    db $00 ; E0 3C
    db $00 ; E0 3D
    db $00 ; E0 3E
    db $00 ; E0 3F "Sleep"
    db $00 ; E0 40 "My Computer"
    db $00 ; E0 41
    db $00 ; E0 42
    db $00 ; E0 43
    db $00 ; E0 44
    db $00 ; E0 45
    db $00 ; E0 46
    db $00 ; E0 47
    db $00 ; E0 48 "E-Mail"
    db $00 ; E0 49
    db $2F ; E0 4A "KP /" ; YYY this is a temporary value for testing
    db $00 ; E0 4B
    db $00 ; E0 4C
    db $00 ; E0 4D "Next Track"
    db $00 ; E0 4E
    db $00 ; E0 4F
    db $00 ; E0 50 "Media Select"
    db $00 ; E0 51
    db $00 ; E0 52
    db $00 ; E0 53
    db $00 ; E0 54
    db $00 ; E0 55
    db $00 ; E0 56
    db $00 ; E0 57
    db $00 ; E0 58
    db $00 ; E0 59
    db $00 ; E0 5A "KP EN"
    db $00 ; E0 5B
    db $00 ; E0 5C
    db $00 ; E0 5D
    db $00 ; E0 5E "Wake"
    db $00 ; E0 5F
    db $00 ; E0 60
    db $00 ; E0 61
    db $00 ; E0 62
    db $00 ; E0 63
    db $00 ; E0 64
    db $00 ; E0 65
    db $00 ; E0 66
    db $00 ; E0 67
    db $00 ; E0 68
    db $b3 ; E0 69 "END"
    db $00 ; E0 6A
    db $93 ; E0 6B "L ARROW"
    db $b2 ; E0 6C "HOME"
    db $00 ; E0 6D
    db $00 ; E0 6E
    db $00 ; E0 6F
    db $b0 ; E0 70 "INSERT"
    db $b1 ; E0 71 "DELETE"
    db $91 ; E0 72 "D ARROW"
    db $00 ; E0 73
    db $92 ; E0 74 "R ARROW"
    db $90 ; E0 75 "U ARROW"
    db $00 ; E0 76
    db $00 ; E0 77
    db $00 ; E0 78
    db $00 ; E0 79
    db $b5 ; E0 7A "PG DN"
    db $00 ; E0 7B
    db $00 ; E0 7C "PRNT SCRN INNER"
    db $b4 ; E0 7D "PG UP"
    db $00 ; E0 7E
    db $00 ; E0 7F

    ; keymod_shift_tbl - map printable ASCII values on keys (as derived
    ; from keycode_tbl) to the values generated with the SHIFT modifier.
    ; Has 96 entries corresponding to $20-$7f, not all of which are on
    ; keys, but it's easier to use a few extra bytes of table instead of
    ; several extra instructions of logic.
keymod_shift_tbl = *
    db $20 ; 20 " "
    db $21 ; 21 "!" (no key)
    db $22 ; 22 "\"" (no key)
    db $23 ; 23 "#" (no key)
    db $24 ; 24 "$" (no key)
    db $25 ; 25 "%" (no key)
    db $26 ; 26 "&" (no key)
    db $22 ; 27 "'" - to 22 "\""
    db $28 ; 28 "(" (no key)
    db $29 ; 29 ")" (no key)
    db $2A ; 2A "*" (no key)
    db $2B ; 2B "+" (no key)
    db $3C ; 2C "," - to 3C "<"
    db $5F ; 2D "-" - to 5F "_"
    db $3E ; 2E "." - to 3E ">"
    db $3F ; 2F "/" - to 3F "?"
    db $29 ; 30 "0" - to 29 ")"
    db $21 ; 31 "1" - to 21 "!"
    db $40 ; 32 "2" - to 40 "@"
    db $23 ; 33 "3" - to 23 "#"
    db $24 ; 34 "4" - to 24 "$"
    db $25 ; 35 "5" - to 25 "%"
    db $5E ; 36 "6" - to 5E "^"
    db $26 ; 37 "7" - to 26 "&"
    db $2A ; 38 "8" - to 2A "*"
    db $28 ; 39 "9" - to 28 "("
    db $3A ; 3A ":" (no key)
    db $3A ; 3B ";" - to 3A ":"
    db $3C ; 3C "<" (no key)
    db $2B ; 3D "=" - to 2B "+"
    db $3E ; 3E ">" (no key)
    db $3F ; 3F "?" (no key)
    db $40 ; 40 "@" (no key)
    db $41 ; 41 "A" (no key)
    db $42 ; 42 "B" (no key)
    db $43 ; 43 "C" (no key)
    db $44 ; 44 "D" (no key)
    db $45 ; 45 "E" (no key)
    db $46 ; 46 "F" (no key)
    db $47 ; 47 "G" (no key)
    db $48 ; 48 "H" (no key)
    db $49 ; 49 "I" (no key)
    db $4A ; 4A "J" (no key)
    db $4B ; 4B "K" (no key)
    db $4C ; 4C "L" (no key)
    db $4D ; 4D "M" (no key)
    db $4E ; 4E "N" (no key)
    db $4F ; 4F "O" (no key)
    db $50 ; 50 "P" (no key)
    db $51 ; 51 "Q" (no key)
    db $52 ; 52 "R" (no key)
    db $53 ; 53 "S" (no key)
    db $54 ; 54 "T" (no key)
    db $55 ; 55 "U" (no key)
    db $56 ; 56 "V" (no key)
    db $57 ; 57 "W" (no key)
    db $58 ; 58 "X" (no key)
    db $59 ; 59 "Y" (no key)
    db $5A ; 5A "Z" (no key)
    db $7B ; 5B "[" - to 7B "{"
    db $7C ; 5C "\\" - to 7C "|"
    db $7D ; 5D "]" - to 7D "}"
    db $5E ; 5E "^" (no key)
    db $5F ; 5F "_" (no key)
    db $7E ; 60 "`" - to 7E "~"
    db $41 ; 61 "a" - capitalize
    db $42 ; 62 "b" - capitalize
    db $43 ; 63 "c" - capitalize
    db $44 ; 64 "d" - capitalize
    db $45 ; 65 "e" - capitalize
    db $46 ; 66 "f" - capitalize
    db $47 ; 67 "g" - capitalize
    db $48 ; 68 "h" - capitalize
    db $49 ; 69 "i" - capitalize
    db $4A ; 6A "j" - capitalize
    db $4B ; 6B "k" - capitalize
    db $4C ; 6C "l" - capitalize
    db $4D ; 6D "m" - capitalize
    db $4E ; 6E "n" - capitalize
    db $4F ; 6F "o" - capitalize
    db $50 ; 70 "p" - capitalize
    db $51 ; 71 "q" - capitalize
    db $52 ; 72 "r" - capitalize
    db $53 ; 73 "s" - capitalize
    db $54 ; 74 "t" - capitalize
    db $55 ; 75 "u" - capitalize
    db $56 ; 76 "v" - capitalize
    db $57 ; 77 "w" - capitalize
    db $58 ; 78 "x" - capitalize
    db $59 ; 79 "y" - capitalize
    db $5A ; 7A "z" - capitalize
    db $7B ; 7B "{" (no key)
    db $7C ; 7C "|" (no key)
    db $7D ; 7D "}" (no key)
    db $7E ; 7E "~" (no key)
    db $7F ; 7F DEL

    ; keymod_control_tbl - map printable ASCII values on keys (as derived
    ; from keycode_tbl) to the values generated with the CONTROL modifier.
    ; Has 96 entries corresponding to $20-$7f, not all of which are on
    ; keys, but it's easier to use a few extra bytes of table instead of
    ; several extra instructions of logic.
keymod_control_tbl = *
    db $00 ; 20 " " - NUL, aka ^@
    db $21 ; 21 "!" (no key)
    db $22 ; 22 "\"" (no key)
    db $23 ; 23 "#" (no key)
    db $24 ; 24 "$" (no key)
    db $25 ; 25 "%" (no key)
    db $26 ; 26 "&" (no key)
    db $27 ; 27 "'" - nothing shown in VT102 ug; xterm ignores control
    db $28 ; 28 "(" (no key)
    db $29 ; 29 ")" (no key)
    db $2A ; 2A "*" (no key)
    db $2B ; 2B "+" (no key)
    db $2C ; 2C "," - nothing shown in VT102 ug; xterm ignores control
    db $2D ; 2D "-" - ditto
    db $2E ; 2E "." - ditto
    db $1F ; 2F "/" - as per VT102 user guide; xterm too
    db $30 ; 30 "0" - nothing shown in VT102 ug; xterm ignores control
    db $31 ; 31 "1" - ditto
    db $00 ; 32 "2" - following xterm; VT102 user guide shows nothing
    db $1b ; 33 "3" - ditto
    db $1c ; 34 "4" - ditto
    db $1d ; 35 "5" - ditto
    db $1e ; 36 "6" - ditto
    db $1f ; 37 "7" - ditto
    db $7f ; 38 "8" - ditto
    db $39 ; 39 "9" - nothing shown in VT102 ug; xterm ignores control
    db $3A ; 3A ":" (no key)
    db $3B ; 3B ";" - nothing shown in VT102 ug; xterm ignores control
    db $3C ; 3C "<" (no key)
    db $3D ; 3D "=" - nothing shown in VT102 ug; xterm ignores control
    db $3E ; 3E ">" (no key)
    db $3F ; 3F "?" (no key)
    db $40 ; 40 "@" (no key)
    db $41 ; 41 "A" (no key)
    db $42 ; 42 "B" (no key)
    db $43 ; 43 "C" (no key)
    db $44 ; 44 "D" (no key)
    db $45 ; 45 "E" (no key)
    db $46 ; 46 "F" (no key)
    db $47 ; 47 "G" (no key)
    db $48 ; 48 "H" (no key)
    db $49 ; 49 "I" (no key)
    db $4A ; 4A "J" (no key)
    db $4B ; 4B "K" (no key)
    db $4C ; 4C "L" (no key)
    db $4D ; 4D "M" (no key)
    db $4E ; 4E "N" (no key)
    db $4F ; 4F "O" (no key)
    db $50 ; 50 "P" (no key)
    db $51 ; 51 "Q" (no key)
    db $52 ; 52 "R" (no key)
    db $53 ; 53 "S" (no key)
    db $54 ; 54 "T" (no key)
    db $55 ; 55 "U" (no key)
    db $56 ; 56 "V" (no key)
    db $57 ; 57 "W" (no key)
    db $58 ; 58 "X" (no key)
    db $59 ; 59 "Y" (no key)
    db $5A ; 5A "Z" (no key)
    db $1B ; 5B "[" - following VT102 ug
    db $1C ; 5C "\\" - ditto
    db $1D ; 5D "]" - ditto
    db $5E ; 5E "^" (no key)
    db $5F ; 5F "_" (no key)
    db $1E ; 60 "`" - following VT102 ug; on xterm this issues NUL
    db $01 ; 61 "a"
    db $02 ; 62 "b"
    db $03 ; 63 "c"
    db $04 ; 64 "d"
    db $05 ; 65 "e"
    db $06 ; 66 "f"
    db $07 ; 67 "g"
    db $08 ; 68 "h"
    db $09 ; 69 "i"
    db $0A ; 6A "j"
    db $0B ; 6B "k"
    db $0C ; 6C "l"
    db $0D ; 6D "m"
    db $0E ; 6E "n"
    db $0F ; 6F "o"
    db $10 ; 70 "p"
    db $11 ; 71 "q"
    db $12 ; 72 "r"
    db $13 ; 73 "s"
    db $14 ; 74 "t"
    db $15 ; 75 "u"
    db $16 ; 76 "v"
    db $17 ; 77 "w"
    db $18 ; 78 "x"
    db $19 ; 79 "y"
    db $1A ; 7A "z"
    db $7B ; 7B "{" (no key)
    db $7C ; 7C "|" (no key)
    db $7D ; 7D "}" (no key)
    db $7E ; 7E "~" (no key)
    db $7F ; 7F DEL

keymod_nonsticky_mods_tbl = *
    ; for nonsticky modifier keys (like "shift") map from the codes found
    ; in keycode_tbl ($80-$82) to the bits used in keymod_state.
    db KEYMOD_STATE_SHIFT   ; value $80, bit $01: shift
    db KEYMOD_STATE_CONTROL ; value $81, bit $80: control
    db KEYMOD_STATE_ALT     ; value $82, bit $02: alt

keymod_sticky_mods_tbl = *
    ; for sticky modifier keys (like "caps lock") map from the codes
    ; found in keycode_tbl ($88-$8a) to the bits used in keymod_state.
    db KEYMOD_STATE_CAPS_LOCK   ; value $88: caps lock
    db KEYMOD_STATE_NUM_LOCK    ; value $89: num lock
    db KEYMOD_STATE_SCROLL_LOCK ; value $8a: scroll lock

keyled_tbl = *
    ; for sticky modifier keys (like "caps lock") map from the codes
    ; found in keycode_tbl ($88-$8a) to the bits used to identify their
    ; LEDs on a PS/2 keyboard.
    db $04 ; value $88: caps lock
    db $02 ; value $89: num lock
    db $01 ; value $8a: scroll lock

IF ENABLE_MENU
    ; menu_data: Contains a description of the menu: how to draw it and
    ; how to handle selected fields.  Format:
;       menu is a chain of bytes
;       most bytes are characters to be displayed (or escape sequences)
;       but with some "markup" as follows:
;       0 - marks the end of the menu data
;       3 - starts a selectable field.  This byte is followed by:
;               2 bytes - code to call to determine if the field is
;                   currently enabled; returns 1 character indicating it
;               2 bytes - code to call when enabling this field
;               1 byte - parameter to pass to the above code, in the
;                   accumulator
;       4 - ends a selectable field
menu_data = *
    ; Baud rate selection.
    asc "Baud rate: "

IF ENABLE_MENU_LOWBAUDS
    db 3
    dw menu_get_baud
    dw menu_set_baud
    db $1a
    asc "300"

    db 4
    asc "  "

    db 3
    dw menu_get_baud
    dw menu_set_baud
    db $19
    asc "600"

    db 4
    asc "  "

    db 3
    dw menu_get_baud
    dw menu_set_baud
    db $18
    asc "1,200"

    db 4
    asc "  "

    db 3
    dw menu_get_baud
    dw menu_set_baud
    db $09
    asc "1,800"

    db 4
    asc "  "

    db 3
    dw menu_get_baud
    dw menu_set_baud
    db $17
    asc "2,400"

    db 4
    asc "  "

    db 3
    dw menu_get_baud
    dw menu_set_baud
    db $08
    asc "3,600"
    ; Linux doesn't like 3600 baud

    db 4
    asc "  "

    db 3
    dw menu_get_baud
    dw menu_set_baud
    db $16
    asc "4,800"

    db 4, 13, 10 ; end of field & of line
    asc "           " ; for alignment

ENDC ;; ENABLE_MENU_LOWBAUDS

    db 3
    dw menu_get_baud
    dw menu_set_baud
    db $15
    asc "9,600"

    db 4
    asc "  "

    db 3
    dw menu_get_baud
    dw menu_set_baud
    db $14
    asc "19,200"

    db 4
    asc "  "

    db 3
    dw menu_get_baud
    dw menu_set_baud
    db $13
    asc "38,400"

    db 4
    asc "  "

    db 3
    dw menu_get_baud
    dw menu_set_baud
    db $04
    asc "57,600"

    db 4
    asc "  "

    db 3
    dw menu_get_baud
    dw menu_set_baud
    db $03
    asc "115,200"

    db 4
    asc "  "

    db 3
    dw menu_get_baud
    dw menu_set_baud
    db $02
    asc "230,400"

    db 4, 13, 10 ; end of field & of line

    ; Flow-control enable
    asc "Flow control: "

    db 3
    dw menu_get_modeflags
    dw menu_set_modeflags
    db $88|MODEFLAGS2_XONXOFF_LOG
    asc "XON/XOFF"

    db 4
    asc "  "

    db 3
    dw menu_get_modeflags
    dw menu_set_modeflags
    db $08|MODEFLAGS2_XONXOFF_LOG
    asc "none"

    db 4, 13, 10 ; end of field & of line

    ; Local echo
    asc "Local echo: "

    db 3
    dw menu_get_modeflags
    dw menu_set_modeflags
    db $08|MODEFLAGS2_NOECHO_LOG
    asc "on" ; "noecho" is off, so echo is on

    db 4
    asc "  "

    db 3
    dw menu_get_modeflags
    dw menu_set_modeflags
    db $88|MODEFLAGS2_NOECHO_LOG
    asc "off" ; "noecho" is on, so echo is off

    db 4, 13, 10 ; end of field & of line

    ; Stripping top bit enable
    asc "Handling of upper-128 bytes received: "

    db 3
    dw menu_get_modeflags
    dw menu_set_modeflags
    db $88|MODEFLAGS2_CLEAR8_LOG
    asc "strip top bit"

    db 4
    asc "  "

    db 3
    dw menu_get_modeflags
    dw menu_set_modeflags
    db $08|MODEFLAGS2_CLEAR8_LOG
    asc "don't strip top bit"

    db 4, 13, 10 ; end of field & of line

    ; Reset the terminal (you can also use the dedicated reset button
    ; if your hardware has one).
    asc "Reboot VTJ-1 firmware (resets all settings): "

    db 3
    dw menu_get_baud ; won't ever show up as enabled, with bogus value below
    dw start ; will 'jsr start' but then start() resets the stack pointer
    db 255 ; bogus value so it won't ever show up as enabled
    asc "GO!"

    db 4, 13, 10 ; end of field & of line

    db 0 ; end of menu data
ENDC ; ENABLE_MENU

    ; powers_tbl: powers of 2, used for identifying specific bit positions
powers_tbl = *
    db $01, $02, $04, $08, $10, $20, $40, $80

END_OF_CODE = *

* = $fffc
    dw start ; reset vector
    dw irqbrk ; interrupt vector
