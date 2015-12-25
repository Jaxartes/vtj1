#!/usr/bin/tclsh
# Copyright (c) 2015 Jeremy Dilatush
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 
# THIS SOFTWARE IS PROVIDED BY JEREMY DILATUSH AND CONTRIBUTORS
# ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL JEREMY DILATUSH OR CONTRIBUTORS
# BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

# vtj1fonter.tcl
# This program is to generate font.mem, a "ROM" image containing a font for
# the VTJ-1 project.  It'll contain up to 256 characters, 8 pixels wide, up
# to 16 pixels tall.

# It can extract font data from some simple binary formats, I'm thinking 
# mainly PSF.  But rather than decode the PSF format, I'll just have the
# byte offsets specified on a per-file basis.  And rather than have UI for
# inputting the offsets, and a file format for storing them, I'll just
# have the information hard coded in this script.  For the amount of use
# I anticipate it getting, it's easiest that way.

# Command line options:
#       out=$name - output file pathname; default newfont.mem
#       fnt=$name - name of font to use; mandatory or else it will
#                   print a list and exit
#       cmt=1 - enable output commenting, which makes it harder for
#               some synthesis tools to read the output but easier for
#               a human
#       img=1 - output a sample image to look at the font; makes
#               out= mandatory; the output is in PBM rawbits form
#       font_256_chars=1 - fonts with up to 256 chars (instead of 128);
#               this also changes the layout of bytes within the font.
#               Use it if and only if you use font_256_chars=1
#               in vtj1-config.txt.
#       verbose=1 - verbose output

# Here is the font database.  A list with three items per font:
#   + name
#       names ending with -vt are fonts that are set up for use with
#       VTJ-1; they have particular characters in particular places.
#       Other names are just for looking at and investigating various
#       fonts.
#   + description / comments
#   + specification
# The specification in turn is a list of entries which describe how
# character data is to be found:
#   + specify where character data is to be loaded:
#       {file $pathname $offset $perrow $perchar $charrows $var}
#           start reading from file $pathname
#           character data starts $offset bytes in
#           add $perrow bytes between pixel rows
#           add $perchar bytes between characters
#           each character has $charrows rows
#           $var specifies format variants:
#               hex - hex file, such as vtj1fonter.tcl's own output
#               bin - plain binary file
#               bin.gz - gzipped binary file
#               cnum - Numeric byte values embedded in what might be C
#                   source code.  Not parsed very intelligently; be careful.
#               bdf - Adobe BDF format.  Not parsed very intelligently; be
#                   careful.
#   + map one or more character definitions from the current file:
#       {chars $count $start}
#           maps $count characters in a row
#           the first of which is $start
#   + map one character definition from the current file:
#       {char $number}
#   + a literal character definition not taken from the file:
#       {lit $byte ...}
#       Each $byte describes a single row of the character.
#       The most significant bit is the leftmost column.
#       {litx $bytes}
#       Alternative to lit in which $bytes is a single string with two
#       hex digits per byte.
#   + fill in with blanks
#       {blanks $count}
#   + comment; ignored
#       {rem ...}
# Characters in VTJ-1 fonts appear in a particular order:
#       0-31: characters 95-126 of the special character set ("0")
#       32-126: characters 32-126 of the US character set ("B")
#       127: instead of an actual character, this position contains
#           a few bytes of font information, as follows:
#           "row 0", bits 0x1f - font height in pixels (10, 12, or 16)
set db {
    lat1u-12
    "Linux console font lat1u-12"
    {
        {file /usr/share/consolefonts/lat1u-12.psf.gz 4 1 12 12 bin.gz}
        {chars 256 0}
    }
    lat1-12
    "Linux console font lat1-12 unmapped"
    {
        {file /usr/share/consolefonts/lat1-12.psf.gz 32 1 12 12 bin.gz}
        {chars 256 0}
    }
    cafe-12
    "cafe.f12 from fntcol16.zip unmapped"
    {
        {file fonts/cafe.f12 0 1 12 12 bin}
        {chars 256 0}
    }
    evga-alt-12
    "evga-alt.f12 from fntcol16.zip unmapped"
    {
        {file fonts/evga-alt.f12 0 1 12 12 bin}
        {chars 256 0}
    }
    square-12
    "square.f12 from fntcol16.zip unmapped"
    {
        {file fonts/square.f12 0 1 12 12 bin}
        {chars 256 0}
    }
    sony-8x16
    "An 8x16 BDF font file from sony, unmapped"
    {
        {file fonts/sony-8x16.bdf 0 1 16 16 bdf}
        {chars 221 0}
    }
    cafe-12-vt
    "cafe.f12 from fntcol16.zip mapped for VT102 character sets"
    {
        {rem not tested in the real thing}
        {file fonts/cafe.f12 0 1 12 12 bin}
        {rem glyphs 0-31: characters 95-126 of special character set ("0")}
        {char 95} {rem 0 (95) underscore; though VT102 manual says blank}
        {char  4} {rem 1 (96) black diamond}
        {litx aaaa5454aaaa5454aaaa0000} {rem 2 (97) checkerboard}
        {litx 8888f88888003e0808080800} {rem 3 (98) H/T}
        {litx f880e08080003e2038202000} {rem 4 (99) F/F}
        {litx 7880808078003c223e242200} {rem 5 (100) C/R}
        {litx 80808080f8003e2038202000} {rem 6 (101) L/F}
        {char 248} {rem 7 (102) degree}
        {char 241} {rem 8 (103) plus over minus}
        {litx 88c8a8988800202020203e00} {rem 7 (104) N/L}
        {litx 8888505020003e0808080800} {rem 8 9105) V/T}
        {char 217} {rem 11 (106) LR corner}
        {char 191} {rem 12 (107) UR corner}
        {char 218} {rem 13 (108) UL corner}
        {char 192} {rem 14 (109) UR corner}
        {char 197} {rem 15 (110) cross}
        {litx ffff00000000000000000000} {rem 16 (111) horizontal line high}
        {litx 0000ffff0000000000000000} {rem 17 (112) horizontal line mid-high}
        {litx 0000000000ffff0000000000} {rem 18 (113) horizontal line middle}
        {litx 0000000000000000ffff0000} {rem 19 (114) horizontal line mid-low}
        {litx 00000000000000000000ffff} {rem 20 (115) horizontal line low}
        {char 195} {rem 21 (116) tee, point to right}
        {char 180} {rem 22 (117) tee, point to left}
        {char 193} {rem 23 (118) tee, point to top}
        {char 194} {rem 24 (119) tee, point to bottom}
        {char 179} {rem 25 (120) vertical line, middle}
        {char 243} {rem 26 (121) less than or equal}
        {char 242} {rem 27 (122) greater than or equal}
        {char 227} {rem 28 (123) pi}
        {litx 000204fe10fe408000000000} {rem 29 (124) not equal}
        {char 156} {rem 30 (125) UK pound}
        {litx 000000000010000000000000} {rem 31 (126) middle dot}

        {rem glyphs 32-126: characters 32-126 of US character set ("B")}
        {chars 95 32}

        {rem glyph 127: font info}
        {litx 0c}

        {rem glyphs 128-255: These are not needed for the VT102's built
         in character sets.  The character set story for later VT models
         seems quite complicated.  The VT102's "alternate character set"
         option would add another 127 glyphs, but I don't see any documentation
         and I suspect they weren't very standard.

         So, what I'm going to do is: just pack in whatever characters
         I feel like from the font that are not included above}
        {chars 2 1} {rem 128-129}
        {chars 27 5} {rem 130-156}
        {chars 29 127} {rem 157-185}
        {chars 22 157} {rem 186-207}
        {chars 10 181} {rem 208-217}
        {chars 19 198} {rem 218-236}
        {chars 8 219} {rem 237-244}
        {chars 4 228} {rem 245-248}
        {char 240} {rem 249}
        {chars 2 246} {rem 250-251}
        {chars 4 251} {rem 252-255}
    }
    lat1-12-vt
    "Linux console font lat1-12 mapped for VT102 character sets"
    {
        {file /usr/share/consolefonts/lat1-12.psf.gz 32 1 12 12 bin.gz}
        {rem glyphs 0-31: characters 95-126 of special character set ("0")}
        {char  95} {rem 0 (95) underscore; though VT102 manual says blank}
        {char   4} {rem 1 (96) black diamond}
        {lit 170 170 84 84 170 170 84 84 170 170} {rem 2 (97) checkerboard}
        {char   5} {rem 3 (98) H/T}
        {char   6} {rem 4 (99) F/F}
        {char   7} {rem 5 (100) C/R}
        {char   8} {rem 6 (101) L/F}
        {char 176} {rem 7 (102) degree}
        {char 177} {rem 8 (103) plus over minus}
        {char  17} {rem 9 (104) N/L}
        {char  18} {rem 10 (105) V/T}
        {char 201} {rem 11 (106) LR corner}
        {char 204} {rem 12 (107) UR corner}
        {char 198} {rem 13 (108) UL corner}
        {char 195} {rem 14 (109) UR corner}
        {char 207} {rem 15 (110) cross}
        {lit 255} {rem 16 (111) horizontal line high (0/12)}
        {lit 0 0 255} {rem 17 (112) horizontal line mid-high(2/12)}
        {lit 0 0 0 0 0 255} {rem 18 (113) horizontal line middle(5/12)}
        {lit 0 0 0 0 0 0 0 0 255} {rem 19 (114) horizontal line mid-low(8/12)}
        {lit 0 0 0 0 0 0 0 0 0 0 0 255} {rem 20 (115) horizontal line low(11/12)}
        {char 199} {rem 21 (116) tee, point to right}
        {char 205} {rem 22 (117) tee, point to left}
        {char 203} {rem 23 (118) tee, point to top}
        {char 206} {rem 24 (119) tee, point to bottom}
        {char 197} {rem 25 (120) vertical line, middle}
        {char  19} {rem 26 (121) less than or equal}
        {char  20} {rem 27 (122) greater than or equal}
        {char  31} {rem 28 (123) pi}
        {char  21} {rem 29 (124) not equal}
        {char 163} {rem 30 (125) UK pound}
        {char 183} {rem 31 (126) middle dot}

        {rem glyphs 32-126: characters 32-126 of US character set ("B")}
        {chars 95 32}

        {rem glyph 127: font info}
        {litx 0c}

        {rem glyphs 128-255: These are not needed for the VT102's built
         in character sets.  The character set story for later VT models
         seems quite complicated.  The VT102's "alternate character set"
         option would add another 127 glyphs, but I don't see any documentation
         and I suspect they weren't very standard.

         So, what I'm going to do is: just pack in whatever characters
         are in the font but not used above}

        {rem and leaving out entirely:
            horizontal lines, I've got my own: 175, 192, 202, 208
            duplicate of space: 0
            like a cedilla on its own: 184
            one of the half lines: 193
        }

        {chars 2 1} {chars 8 9} {chars 9 22} {rem 128-146}
        {chars 36 127} {chars 11 164} {chars 5 178} {rem 147-198}
        {chars 7 185} {char 194} {char 196} {char 200} {rem 199-208}
        {chars 47 209} {rem 209-255}
    }
    freebsd-boot-font
    "FreeBSD boot_font.c"
    {
        {file fonts/boot_font.c 2 1 16 16 cnum}
        {chars 255 0} {rem the font has 255 entries}
        {litx 90482412090606091224489060609048} {rem just filler I made up}
    }
    freebsd-boot-font-vt
    "FreeBSD boot_font.c mapped for VT102 character sets"
    {
        {rem not tested in the real thing}
        {file fonts/boot_font.c 2 1 16 16 cnum}
        {rem glyphs 0-31: characters 95-126 of special character set ("0")}
        {char 95} {rem 0 (95) underscore; though VT102 manual says blank}
        {char  4} {rem 1 (96) black diamond}
        {litx aaaa5454aaaa5454aaaa5454aaaa0000} {rem 2 (97) checkerboard}
        {litx 8888f888880000003e08080808000000} {rem 3 (98) H/T}
        {litx f880e080800000003e20382020000000} {rem 4 (99) F/F}
        {litx 78808080780000003c223e2422000000} {rem 5 (100) C/R}
        {litx 80808080f80000003e20382020000000} {rem 6 (101) L/F}
        {char 248} {rem 7 (102) degree}
        {char 241} {rem 8 (103) plus over minus}
        {litx 88c8a89888000000202020203e000000} {rem 7 (104) N/L}
        {litx 88885050200000003e08080808000000} {rem 8 9105) V/T}
        {char 217} {rem 11 (106) LR corner}
        {char 191} {rem 12 (107) UR corner}
        {char 218} {rem 13 (108) UL corner}
        {char 192} {rem 14 (109) UR corner}
        {char 197} {rem 15 (110) cross}
        {litx ff000000000000000000000000000000} {rem 16 (111) hz line high}
        {litx 00000000ff0000000000000000000000} {rem 17 (112) hz line mid-high}
        {litx 0000000000000000ff00000000000000} {rem 18 (113) hz line middle}
        {litx 000000000000000000000000ff000000} {rem 19 (114) hz line mid-low}
        {litx 000000000000000000000000000000ff} {rem 20 (115) hz line low}
        {char 195} {rem 21 (116) tee, point to right}
        {char 180} {rem 22 (117) tee, point to left}
        {char 193} {rem 23 (118) tee, point to top}
        {char 194} {rem 24 (119) tee, point to bottom}
        {char 179} {rem 25 (120) vertical line, middle}
        {char 243} {rem 26 (121) less than or equal}
        {char 242} {rem 27 (122) greater than or equal}
        {char 227} {rem 28 (123) pi}
        {litx 00000002047e08107e20400000000000} {rem 29 (124) not equal}
        {char 156} {rem 30 (125) UK pound}
        {litx 00000000000000181800000000000000} {rem 31 (126) middle dot}

        {rem glyphs 32-126: characters 32-126 of US character set ("B")}
        {chars 95 32}

        {rem glyph 127: font info}
        {litx 10}

        {rem glyphs 128-255: These are not needed for the VT102's built
         in character sets.  The character set story for later VT models
         seems quite complicated.  The VT102's "alternate character set"
         option would add another 127 glyphs, but I don't see any documentation
         and I suspect they weren't very standard.

         So, what I'm going to do is: just pack in whatever characters
         I feel like from the font that are not included above}
        {chars 2 1} {rem 127-129}
        {chars 27 5} {rem 130-156}
        {chars 29 127} {rem 157-185}
        {chars 22 157} {rem 186-207}
        {chars 10 181} {rem 208-217}
        {chars 19 198} {rem 218-236}
        {chars 8 219} {rem 237-244}
        {chars 4 228} {rem 245-248}
        {char 240} {rem 249}
        {chars 2 246} {rem 250-251}
        {chars 4 251} {rem 252-255}
    }
    sony-8x16-vt
    "An 8x16 BDF font file from sony, mapped for VT102 character sets"
    {
        {file fonts/sony-8x16.bdf 0 1 16 16 bdf}
        {rem this doesn't need as much mapping as the others}

        {char 94} {rem 0 (95) underscore; though VT102 manual says blank}
        {chars 126 0} {rem 1-126 almost unmapped, just shifted by 1}
        {litx 10} {rem 127 font info}

        {rem glyphs 128-255: These are not needed for the VT102's built
         in character sets.  The character set story for later VT models
         seems quite complicated.  The VT102's "alternate character set"
         option would add another 127 glyphs, but I don't see any documentation
         and I suspect they weren't very standard.

         So, what I'm going to do is: just pack in whatever characters
         I feel like from the font that are not included above}
        {chars 93 128} {rem 128-220 unmapped.}
        {rem That's all the font has.  There's room for 35 more characters,
             perhaps hand coded, or from another font.}
    }
}

# Handlers for various input formats.  The function named inhandler_$fmt is
# passed a file name, and it in turn fills up an array passed to it by
# name, with byte values from the file.

proc inhandler_bin {fn ary} {
    # This handler reads binary bytes

    upvar $ary bytes
    array unset bytes

    set fp [open $fn r]
    fconfigure $fp -encoding binary -translation binary

    set dmax 65536
    set data [read $fp $dmax]
    set l [string length $data]
    if {$l >= $dmax} {
        error "Input file is too big!"
    }
    for {set i 0} {$i < $l} {incr i} {
        set bytes($i) [scan [string index $data $i] %c]
    }
    set bytes() $l
    close $fp
}

proc inhandler_hex {fn ary} {
    # This handler reads hex bytes.  Its syntax is fairly limited.
    # It can handle this program's own output, that's about all.

    upvar $ary bytes
    array unset bytes
    set bytes() 0

    set fp [open $fn r]

    while {1} {
        # get a line
        set line [gets $fp]
        if {$line eq "" && [eof $fp]} {
            break
        }

        # remove comments and liminal whitespace
        set first [string first "//" $line]
        if {$first >= 0} {
            set line [string range $string 0 ${first}-1]
        }
        set line [string trim $line]

        # break it into "words" and process each one as a byte
        foreach $word [split $line " \t\r\014"] {
            if {[string first "@" $word] >= 0} {
                error "input word contains @ (address), not supported"
            } elseif {![string is xdigit -strict $word]} {
                error "input word is not hexadecimal, not supported"
            } elseif {![string length $word] > 4} {
                error "input word is too long, not supported"
            } else {
                set scanned [scan $word %x%s]
                if {[llength $scanned] < 1 || [lindex $scanned 1] ne ""} {
                    error "input word could not be parsed as hex"
                }
                set byte [lindex $scanned 0]
                if {$byte < 0 || $byte > 255} {
                    error "input word contained out of range byte value $byte"
                }
                set bytes($bytes()) $byte
                incr bytes()
            }
        }
    }

    close $fp
}

proc inhandler_cnum {fn ary} {
    # Another rather limited text reader.  It extracts numbers representing
    # byte values, not found inside comments.  It's just about enough
    # to read "boot_font.c".

    upvar $ary bytes
    array unset bytes
    set bytes() 0

    set fp [open $fn r]

    # Read the entire file into a buffer in memory.
    set data [read $fp 1048576]
    close $fp

    # Convert all comments and whitespace sequences into single spaces,
    # which is what they're synonymous with in C.  And all string constants
    # too; we don't care about them.
    set re {([[:space:]]|[/][*]([^*]|[*][^/])*[*][/]|"([^"]|\\.)*")+}
    set data [regsub -all -expanded -- $re $data " "]

    # Replace any non alphanumeric characters with spaces.
    set re {[^[:alnum:]]}
    set data [regsub -all -expanded -- $re $data " "]

    # Now split it into words and scan the words for numbers in the right
    # range.
    foreach word [split $data " "] {
        if {[string is integer -strict $word] ||
            (([string match 0x* $word] || [string match 0X* $word]) &&
             [string is xdigit -strict [string range $word 2 end]])} {
            if {$word >= 0 && $word < 256} {
                set bytes($bytes()) $word
                incr bytes()
            }
        }
    }
}

proc inhandler_bin.gz_trf {fn ary} {
    # This version of inhandler_bin.gz doesn't seem to work.
    # Every read operation gets "invalid argument"

    # This handler reads a binary file compressed with gzip.  It needs to
    # have the Trf Tcl package installed
    package require Tcl 8.2
    package require Trf 2.1.4

    upvar $ary bytes
    array unset bytes

    set fp [open $fn r]
    fconfigure $fp -encoding binary -translation binary

    # The -mode parameter to zip specifies the operation for writing; since
    # we're reading, "-mode compress" means decompress.  Whee.
    zip -mode compress -attach $fp
    inhandler_bin $fp bytes

    close $fp
}

proc inhandler_bin.gz {fn ary} {
    # This handler reads a binary file compressed with gzip.  It uses
    # the "zcat" external program.  (This could also be done by a package
    # within Tcl, "Trf", but I couldn't seem to get it to work; see
    # inhandler_bin.gz_trf.)

    upvar $ary bytes
    array unset bytes
    set bytes() 0

    set fp [open "| zcat $fn" r]
    fconfigure $fp -encoding binary -translation binary

    set dmax 65536
    set data [read $fp $dmax]
    set l [string length $data]
    if {$l >= $dmax} {
        error "Input file is too big!"
    }
    for {set i 0} {$i < $l} {incr i} {
        set bytes($i) [scan [string index $data $i] %c]
    }
    set bytes() $l
    close $fp
}

proc inhandler_bdf {fn ary} {
    # Handle font input in "BDF" format; see:
    # https://partners.adobe.com/public/developer/en/font/5005.BDF_Spec.pdf
    # This doesn't decode the entire format, just a minimal subset
    # in order to get the font bitmap data into VTJ-1.

    # It expects fonts no more than 8 pixels wide, and 16 pixels high,
    # and stores them all as though they were 8 pixels wide & 16 high.

    # Set up to read the font and store its data into an array
    global opts
    upvar $ary bytes
    array unset bytes
    set bytes() 0
    set fp [open $fn r]
    puts stderr "Reading font input from BDF file $fn"

    # Read font data; mainly we're interested in bitmaps and we'll ignore
    # most other data.
    set char_name ""
    set char_bbx ""
    set char_bitmap ""
    set bitmaptogo 0
    while {![eof $fp]} {
        set line [string trim [gets $fp]]
        if {$bitmaptogo > 0} {
            lappend char_bitmap [scan $line %x]
            incr bitmaptogo -1
        } elseif {[string match -nocase "FONT *" $line]} {
            puts stderr "Input font name is: [string range $line 5 end]"
        } elseif {[string match -nocase "COPYRIGHT *" $line]} {
            puts stderr "Input font copyright is: [string range $line 10 end]"
        } elseif {[string match -nocase "STARTCHAR *" $line]} {
            if {$char_name ne ""} {
                error "BDF format error: STARTCHAR, without ENDCHAR"
                # talking about the *previous* STARTCHAR, not this one
            }
            set char_name [string range $line 10 end]
        } elseif {[string match -nocase "BITMAP" $line]} {
            if {$char_name eq ""} {
                error "BDF format error: BITMAP without STARTCHAR"
            }
            if {$char_bitmap ne ""} {
                error "BDF format error: BITMAP twice in a character"
            }
            if {$char_bbx eq ""} {
                error "BDF format error: BITMAP without BBX"
            }
            lassign $char_bbx xpix ypix otherstuff
            set bitmaptogo $ypix
        } elseif {[string match -nocase "BBX *" $line]} {
            if {$char_name eq ""} {
                error "BDF format error: BBX without STARTCHAR"
            }
            if {$char_bbx ne ""} {
                error "BDF format error: BBX twice in a character"
            }
            set char_bbx [string range $line 4 end]
            lassign $char_bbx xpix ypix
            if {![string is integer -strict $xpix] ||
                ![string is integer -strict $ypix] ||
                $xpix < 1 || $ypix < 1} {
                error "BDF format error: invalid BBX values"
            }
            if {$xpix > 8 || $ypix > 16} {
                error "BDF format error: BBX values vtj1fonter cannot handle"
            }
        } elseif {[string match -nocase "ENDCHAR" $line]} {
            if {$char_bitmap eq ""} {
                error "BDF format error: ENDCHAR without BITMAP; vtj1fonter needs BITMAP"
            }
            # If it has a bitmap, it has the other stuff too.
            if {$opts(verbose)} {
                puts stderr \
                    "Have read character '$char_name', putting at $bytes()"
            }
            for {set i 0} {$i < 16} {incr i} {
                set byte [lindex $char_bitmap $i]
                if {$byte eq ""} {
                    set byte 0
                } else {
                    set byte [expr {$byte & 255}]
                }
                set bytes($bytes()) $byte
                incr bytes()
            }
            set char_name ""
            set char_bbx ""
            set char_bitmap ""
        }
    }
    close $fp
}

# Code which reads command line options and processes them.
set opts(out) ""
set opts(fnt) ""
set opts(cmt) ""
set opts(img) ""
set opts(font_256_chars) 0
set opts(verbose) 0
foreach arg $argv {
    set eq [string first = $arg]
    if {$eq < 0} {
        error "Bad option '$arg'"
    }
    set n [string range $arg 0 ${eq}-1]
    set v [string range $arg ${eq}+1 end]
    if {![info exists opts($n)]} {
        error "Unrecognized option '$n'"
    }
    set opts($n) $v
}

if {$opts(img) eq ""} { set opts(img) 0 }
if {$opts(cmt) eq ""} { set opts(cmt) 0 }
if {$opts(fnt) eq ""} {
    puts stderr "Available font names and descriptions:"
    set db2() [list]
    foreach {n d s} $db {
        lappend db2() $n
        set db2($n) $d
    }
    foreach n [lsort $db2()] {
        puts "\t$n"
        foreach l [split $db2($n) "\n"] {
            puts "\t\t$l"
        }
    }
    error "Must specify fnt=..."
}
if {$opts(img) && $opts(out) eq ""} {
    error "with img=1, out=... becomes mandatory"
}
if {$opts(out) eq ""} {
    set opts(out) newfont.mem
}
if {$opts(font_256_chars) eq "0"} {
    set opts(ccwid) 7 ; # 128 chars in 2kB
} elseif {$opts(font_256_chars) eq "1"} {
    set opts(ccwid) 8 ; # 256 chars in 4kB
} else {
    error "0 and 1 are the only allowed values for font_256_chars="
}
foreach {n d s} $db {
    # looking for a matching font entry
    if {$n eq $opts(fnt)} {
        set opts(fntd) $d
        set opts(fnts) $s
        break
    }
}
if {![info exists opts(fnts)]} {
    error "font '$opts(fnt)' not found"
}

# Initialize our internal record of font data.  It's 4k bytes in an array
for {set i 0} {$i < 4096} {incr i} {
    set img($i) 0
}
set genpos 0

# Now, go through the font specification and put data in our internal record.
proc storechar {bytes src} {
    # store a character in the font, with each of $bytes giving one row
    global genpos img opts
    if {$genpos >> $opts(ccwid)} {
        if {$opts(verbose)} {
            puts stderr "Ignoring char $genpos - from $src - too high"
        }
        incr genpos
        return
    }
    set a $genpos
    foreach byte $bytes {
        set img($a) $byte
        incr a [expr {1<<$opts(ccwid)}]
    }
    if {$opts(verbose)} {
        puts stderr "Stored char $genpos - from $src"
    }
    incr genpos
}
proc mapchar {inpos} {
    # map character $inpos from the input file, to current character of the
    # output
    global inf infb

    set bytes [list]
    for {set i 0} {$i < $inf(charrows)} {incr i} {
        set a [expr {$inf(offset)+$inf(perrow)*$i+$inf(perchar)*$inpos}]
        lappend bytes $infb($a)
    }
    storechar $bytes "char $inpos of $inf(pathname)"
}

foreach cmd $opts(fnts) {
    switch -- [lindex $cmd 0] {
        file {
            # read an input file
            # the results go into two arrays, inf(...) and infb(...)
            lassign $cmd op pathname offset perrow perchar charrows variant
            set inf(pathname) $pathname
            set inf(offset) $offset
            set inf(perrow) $perrow
            set inf(perchar) $perchar
            set inf(charrows) $charrows
            inhandler_$variant $pathname infb
            puts stderr "Have read input file $pathname - variant $variant"
            puts stderr "it contained $infb() bytes"
        }
        chars {
            # map one or more characters from file
            lassign $cmd op count start
            for {set i 0} {$i < $count} {incr i} {
                mapchar [expr {$i + $start}]
            }
        }
        char {
            # map one character from file
            lassign $cmd op num
            mapchar $num
        }
        lit {
            # literal character data, doesn't come from file
            storechar [lrange $cmd 1 end] "literal"
        }
        litx {
            # literal character data, doesn't come from file, is in hex
            set hex [lindex $cmd 1]
            if {[string length $hex] < 1 || ([string length $hex] % 2) ||
                ![string is xdigit -strict $hex]} {
                error "Bad hex data $hex"
            }
            set bytes [list]
            for {set i 0} {$i < [string length $hex]} {incr i 2} {
                lappend bytes [scan [string range $hex $i 1+$i] %02x]
            }
            storechar $bytes "literal"
        }
        blanks {
            lassign $cmd op count
            for {set i 0} {$i < $count} {incr i} {
                storechar {} "blank"
            }
        }
        rem {
            # comment, is ignored
        }
        default {
            # unknown directive
            error "Unknown directive '[lindex $cmd 0]' in font spec"
        }
    }
}

# Now the font has been built up in $img(...); write it out
set fp [open $opts(out) w]
if {$opts(img)} {
    # image output: 32x8 array of 8x16 characters
    fconfigure $fp -translation binary -encoding binary
    puts -nonewline $fp [format "P4\n%d %d\n" 256 128]
    for {set y 0} {$y < (1<<($opts(ccwid)-1))} {incr y} {
        for {set x 0} {$x < 256} {incr x 8} {
            set cy [expr {$y & 15}]
            set ch [expr {(($x >> 3) & 31) + ((($y >> 4) & 7) << 5)}]
            set b $img([expr {$ch + ($cy << $opts(ccwid))}])
            puts -nonewline $fp [format %c $b]
        }
    }
} else {
    # hex output, with or without comments
    for {set a 0} {$a < (16<<$opts(ccwid))} {incr a} {
        set l [format %02x $img($a)]
        if {$opts(cmt) && !($a & 7)} {
            append l [format { // relative address 0x%04x: char %02x row %02x} \
                $a [expr {$a & ((1<<$opts(ccwid))-1)}] \
                [expr {$a >> $opts(ccwid)}]]
        }
        puts $fp $l
    }
}
close $fp

# done
exit 0
