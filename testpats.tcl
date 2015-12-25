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

# Generate and write out a few test patterns for vtj1.asm.  The general syntax
# is: sudo tclsh testpats.tcl <patname> [options].
# Some test patterns are meant to be compared against another terminal
# such as "xterm".  Others have some sort of internal consistency or
# expected pattern to look for.

# Each pattern is accompanied by a comment giving some information about
# what to do or to look for.  When comparing against another terminal,
# the screen dimensions can and do make a difference.  If something seems
# mismatched, also, try it with "reset".

# Some options that may be of interest:
#   stdout - run in the current terminal (not necessarily VTJ-1) instead
#            of /dev/ttyUSB1; recommend also using "linger" in this case
#   reset - reset the terminal (& wait a few seconds) before making the
#           pattern.  This is of use if the previous pattern has left
#           the terminal in an usual state.
#   linger - wait a few minutes, after showing the pattern, before returning
#            you to the prompt.
#   "srand $seed" - Specify an integer seed for pseudorandom number
#           generation.  Some patterns will show up a little different
#           with different seed values.
#   "parm $param" - Specify an integer parameter value for the pattern.
#           Most patterns ignore this, but a few will work differently
#           with different parameter values.
#   "dev $filename" - Specify a filename (probably in /dev) by which to
#           access the serial port.
#   "height $value" - height of screen in rows.  Typically 30 or 40,
#           it depends on the font you use.

set dev /dev/ttyUSB1
set baud 115200
set pattern [lindex $argv 0]
set reset 0
set resetms 15000
set linger 0
set highcsi 0
set slowdown 0
set xonxoff 1
set parameter 0
set height 40
expr {srand(14703)}
foreach mod [lrange $argv 1 end] {
    if {$mod eq "stdout"} {
        set dev ""
    } elseif {$mod eq "reset"} {
        set reset 1
    } elseif {$mod eq "fastreset"} {
        set reset 1
        set resetms 1000
    } elseif {$mod eq "linger"} {
        set linger 1
    } elseif {$mod eq "random"} {
        set seed [clock milliseconds]
        set fp [open /dev/urandom r]
        set bytes [read $fp 8]
        close $fp
        binary scan $bytes w seed
        set seed [expr {$seed & 0x7fffffff}]
        puts stderr "seed = $seed"
        expr {srand($seed)}
    } elseif {[string match "srand *" $mod]} {
        expr {srand([lindex $mod 1])}
    } elseif {[string match "parm *" $mod]} {
        set parameter [lindex $mod 1]
    } elseif {[string match "dev *" $mod]} {
        set dev [lindex $mod 1]
    } elseif {[string match "height *" $mod]} {
        set height [lindex $mod 1]
    } elseif {$mod eq "nonlcr"} {
        # Suppress the '\n' -> '\r\n' translation; use only with "stdout".
        # Added for use with "rightedge".  Also use with "lfnl".
        exec stty -onlcr
        fconfigure stdout -translation lf
    } elseif {$mod eq "highcsi"} {
        # Replace the control sequencer introducer, 27 91 (^[[) with
        # a one byte alternative, 155, some of the time.
        # When using this with the 'xterm' terminal, give 'xterm' the
        # '+u8' option so that it doesn't mistake it for a utf-8 wide char.
        set highcsi 0.5
    } elseif {$mod eq "highcsialways"} {
        # Like "highcsi" but does it always.
        set highcsialways 1.0
    } elseif {$mod eq "noxonxoff"} {
        # Disable XON/XOFF flow control
        set xonxoff 0
    } elseif {$mod eq "slowdown"} {
        # Add a 5ms pause between bytes.
        incr slowdown 5
    } elseif {[string match "baud *" $mod]} {
        # set baud rate other than 115200
        set baud [string range $mod 5 end]
    } else {
        puts stderr "Unknown keyword $mod, ignored."
    }
}

puts stderr "dev = $dev"
puts stderr "baud = $baud"
puts stderr "height = $height"
puts stderr "pattern = $pattern"

if {$dev eq ""} {
    set fp stdout
} else {
    set fp [open $dev r+]
    fconfigure $fp -mode ${baud},n,8,1
}
fconfigure $fp -buffering none -encoding binary -translation binary -blocking 1
if {$dev ne ""} {
    fconfigure $fp -handshake [expr {$xonxoff ? "xonxoff" : "none"}]
}

# randomize - utility function to sort a list in pseudorandom order
proc randomize {lst} {
    # Build a list that includes pseudorandom "key" numbers
    set lst2 [list]
    foreach elt $lst {
        lappend lst2 [list [expr {rand()}] $elt]
    }

    # And sort by that
    set lst3 [lsort -real -index 0 $lst2]

    # And extract the original elements
    set lst4 [list]
    foreach elt $lst3 {
        lappend lst4 [lindex $elt 1]
    }

    return $lst4
}

# pat_* - build and return a string giving the specified pattern.
# string can contain text, or any escape sequences the terminal recognizes,
# or the following special code:
#       \033[==number| - delay number milliseconds

set patterns(colors) "Foreground and background colors, and inverse video"
# "colors": Compare against another terminal & what looks reasonable.  Each
# of the eight colors is used once in each row, with the letter 'a'.
proc pat_colors {} {
    set s ""
    set csi [format "%c\[" 27]
    set crlf [format "%c%c" 13 10]
    append s "${csi}m${crlf}${crlf}"
    append s "Foreground: . . . . . . . . . . "
    for {set c 0} {$c < 7} {incr c} {
        append s "${csi}3${c}ma"
    }
    append s "${csi}m${crlf}"
    append s "Foreground, inverted: . . . . . "
    append s "${csi}7m"
    for {set c 0} {$c < 7} {incr c} {
        append s "${csi}3${c}ma"
    }
    append s "${csi}m${crlf}"
    append s "Background: . . . . . . . . . . "
    for {set c 0} {$c < 7} {incr c} {
        append s "${csi}4${c}ma"
    }
    append s "${csi}m${crlf}"
    append s "Background, inverted: . . . . . "
    append s "${csi}7m"
    for {set c 0} {$c < 7} {incr c} {
        append s "${csi}4${c}ma"
    }
    append s "${csi}m${crlf}"

    append s "${crlf}${crlf}"
    return $s
}

set patterns(colors2) "Foreground and background colors, and bold, in grid"
# "colors2": Shows a grid with the letter 'a' in various combinations of
# foreground and background colors and bold.  Can be compared against
# expectation or against another terminal.
proc pat_colors2 {} {
    return [pat_colors2x {0 1 2 3 4 5 6 7 8 9} {0 1 2 3 4 5 6 7 8 9}]
}
# "colors2r": Like "colors2", but the color values are not in order.
set patterns(colors2r) "Foreground and background colors, and bold, in grid, reordered"
proc pat_colors2r {} {
    return [pat_colors2x {0 3 6 9 2 5 8 1 4 7} {0 3 6 9 2 5 8 1 4 7}]
}
proc pat_colors2x {fgs bgs} {
    set s ""
    set csi [format "%c\[" 27]
    set crlf [format "%c%c" 13 10]

    # initial preparation
    append s "${csi}m${crlf}${crlf}"

    # top legend (foregrounds)
    append s "  "
    foreach fg $fgs {
        append s $fg
    }
    append s " ${csi}1m"
    foreach fg $fgs {
        append s $fg
    }
    append s "${csi}m${crlf}"

    # main body, and left legend (backgrounds)
    foreach bg $bgs {
        append s " $bg${csi}4${bg}m"
        foreach fg $fgs {
            append s "${csi}3${fg}ma"
        }
        append s " ${csi}1m"
        foreach fg $fgs {
            append s "${csi}3${fg}ma"
        }
        append s " ${csi}m${crlf}"
    }

    append s $crlf$crlf
    return $s
}

set patterns(chars) "Characters with VT102 equivalents"
# "chars": Shows the characters found in the VT102's three built in
# character sets.  You can compare against another terminal.
proc pat_chars {} {
    set s ""
    set esc [format %c 27]
    append s "${esc}(B" ; # US character set as G0
    append s "${esc})0" ; # GFX character set as G1
    set i 0
    for {set r 0} {$r < 8} {incr r} {
        for {set c 0} {$c < 16} {incr c ; incr i} {
            if {$i == 0} {
                append s [format %c 14] ; # graphics character set
            } elseif {$i == 32} {
                append s [format %c 15] ; # US character set
            } elseif {$i == 127} {
                append s "${esc})A"
                append s [format %c 14] ; # UK character set
            }
            if {$i < 32} {
                append s [format %c [expr {95+$i}]]
            } elseif {$i < 127} {
                append s [format %c $i]
            } else {
                append s "\#"
            }
        }
        append s "\r\n"
    }
    append s [format %c 15] ; # normal character set
    return $s
}

set patterns(charsets) "Character set display"
# "charsets": Shows the contents of the various recognized character sets.
# Not all the same as other terminals; but "UK", "US" and "gfx" are meant to
# be the same as documented in the VT102 manual.
proc pat_charsets {} {
    set s ""
    set esc [format "%c" 27]
    set norm "\017${esc}(B${esc})B\017"
    set crlf [format "%c%c" 13 10]

    append s $crlf$crlf

    foreach {mn mx} {32 79 80 126} {
        foreach {sl slc sls} [list G0 [format %c 15] "${esc}(" \
                                   G1 [format %c 14] "${esc})"] {
            foreach {cs csc} {UK A US B gfx 0 alt 1 altspc 2} {
                append s $norm
                set m "Set $cs used as ${sl}, ${mn}-${mx}"
                append s [format "%29s: " $m]
                append s $slc$sls$csc
                for {set i $mn} {$i <= $mx} {incr i} {
                    append s [format %c $i]
                }
                append s $crlf
            }
        }
    }
    append s $norm$crlf
    return $s
}

set patterns(charsetchg) "Character set switching"
# "charsetchg": Tests switching from one character set to another.  The lines
# of characters should match within their groups of four.  This is randomized,
# so may be altered with "srand $seed".
proc pat_charsetchg {} {
    set s ""
    set esc [format "%c" 27]
    set norm "\017${esc}(B${esc})B\017"
    set crlf [format "%c%c" 13 10]

    set ln ""
    for {set i 32} {$i < 47} {incr i} {append ln [format %c $i]}
    for {set i 64} {$i < 126} {incr i} {append ln [format %c $i]}

    set sets {A B 0 1 2}
    set nsets [llength $sets]

    append s $crlf$crlf
    foreach cset {A B 0} {
        append s $crlf$norm
        append s "Set ${cset}: The following four lines should match:"

        # First time: simply
        append s "\017$crlf${esc}(${cset}${esc})${cset}\017"
        append s $ln

        # Second through fourth times: pseudorandom ways
        foreach _ {a b c} {
            # start at a reasonable default
            append s $crlf$norm
            set set(G0) B
            set set(G1) B
            set cur G0

            # now display all characters
            foreach ch [split $ln ""] {
                # before displaying the character, mix up the character
                # sets a little, but make sure to use the right one
                while {($set($cur) ne $cset) || int(rand()*1.5)} {
                    if {int(rand()*2)} {
                        # switch between G0 & G1
                        if {$cur eq "G0"} {
                            set cur G1
                            append s "\016"
                        } else {
                            set cur G0
                            append s "\017"
                        }
                    } else {
                        # change G0 or G1
                        set chg [expr {int(rand()*2)}]
                        set nset [lindex $sets [expr {int(rand()*$nsets)}]]
                        append s [format %s%s%s \
                            $esc \
                            [expr {$chg ? ")" : "("}] \
                            $nset]
                        set set([expr {$chg ? "G1" : "G0"}]) $nset
                    }
                }
                append s $ch
            }
        }
    }

    append s $norm$crlf$crlf
    return $s
}

# boxchars: database of box drawing characters indexed by
#   N=1 S=2 W=4 E=8
set boxchars " ??x?jku?mltqvwn"

set patterns(boxes) "Boxes drawn with box drawing characters"
# "boxes": Draws a pattern of boxes using the graphic character set.
# May be compared against another terminal.  Also visually check that the
# longer lines are straight.
proc pat_boxes {} {
    global boxchars

    # setup
    set s ""
    append s "\033)0" ; # set G1 character set to the graphic set
    append s "\016" ; # switch to the G1 character set

    if {0} {
        # (unused code)
        # designate areas pseudorandomly; they're associated with the
        # corners between characters
        set xdim 30
        set ydim 20
        for {set y 0} {$y <= $ydim} {incr y} {
            for {set x 0} {$x <= $xdim} {incr x} {
                set grid($x,$y) [expr {int(rand()*3)}]
            }
        }
    } else {
        # make a consistent pattern of "areas"
        set xpat {0 1 2 2 1 0 2 2 1 0 1 0 0 1 2 2 0 1 1 1 0 2 1 2 }
        set ypat $xpat
        set xdim [llength $xpat]
        set ydim [llength $ypat]
        for {set y 0} {$y < $ydim} {incr y} {
            for {set x 0} {$x < $xdim} {incr x} {
                set grid($x,$y) \
                    [expr {[lindex $xpat $x] * [lindex $ypat $y]}]
            }
        }
    }
    for {set y 0} {$y <= $ydim} {incr y} {
        set grid(0,$y) 0
        set grid($xdim,$y) 0
    }
    for {set x 0} {$x <= $xdim} {incr x} {
        set grid($x,0) 0
        set grid($x,$ydim) 0
    }

    # and draw a grid giving the boundaries between them
    for {set y 0} {$y < $ydim} {incr y} {
        for {set x 0} {$x < $xdim} {incr x} {
            set xx $x ; incr xx
            set yy $y ; incr yy
            set lb 0 ; # "line bits"
            if {$grid($x,$y) != $grid($xx,$y)} {
                set lb [expr {$lb | 1}] ; # north
            }
            if {$grid($x,$yy) != $grid($xx,$yy)} {
                set lb [expr {$lb | 2}] ; # south
            }
            if {$grid($x,$y) != $grid($x,$yy)} {
                set lb [expr {$lb | 4}] ; # west
            }
            if {$grid($xx,$y) != $grid($xx,$yy)} {
                set lb [expr {$lb | 8}] ; # east
            }
            if {$lb == 0 && $grid($x,$y)} {
                append s "a" ; # checkerboard
            } else {
                append s [string index $boxchars $lb]
            }
        }
        append s "\r\n"
    }

    # and finish up
    append s "\017" ; # switch back to G0 character set
}

set patterns(chardel1) "Character deletion producing matching lines"
# chardel1: The two lines in each pair of lines, should match exactly.
# This is pseudorandom and may be altered with "srand $seed".
proc pat_chardel1 {} {
    # Assumes: default tab stops
    set s ""

    set blackfill 1
    set e [format %c 27]
    set crlf [format %c%c 13 10]
    set cr [format %c 13]
    set t [format %c 9]
    set bs [format %c 8]

    # First pair of lines: alphabet and a little attributes

    append s $crlf
    # First line: desired appearance without using deletion
    append s $crlf
    append s "${e}\[m"
    append s "abcdefgijklmnopqstuvwyxzX"
    append s "${e}\[31;4m"
    append s "efghijklmnopq"
    append s "${e}\[m"
    append s "_"
    append s "${e}\[;41m"
    append s "abcdefghijklwyxz"
    append s "${e}\[1m"
    append s "ab"
    append s "${e}\[m"

    # Second line: uses deletion
    append s $crlf
    append s "${e}\[m"
    append s "abcdefghijklmnopqrstuvwyxz"
    append s "${e}\[4;31m"
    append s "abcdefghijklmnopqrstuvwyxz"
    append s "${e}\[;41m"
    append s "abcdefghijklmnopqrstuvwyxz"
    append s "${e}\[1m"
    append s "ab"
    append s "${e}\[m"
    append s $cr
    append s "${t}${bs}${e}\[P"
    append s "${t}${t}${e}\[1P"
    append s "${t}X${e}\[3P"
    append s "${t}${t}${bs}${bs}${e}\[8P_"
    append s $cr
    append s "${t}${t}${t}${t}${t}${t}${t}${bs}${bs}${bs}${bs}${bs}${e}\[10P"

    # Second through fifth pair of lines: background color
    foreach dlim {3 5 7 20} {
        append s $crlf
        set bgs0 [list]
        set chs0 [list]
        for {set i 0} {$i < 80} {incr i} {
            lappend bgs0 [expr {int(rand()*6)}]
            lappend chs0 x
        }
        set chs1 $chs0
        set bgs1 $bgs0
        set dels [list]
        for {set i 0} {$i < 80} {incr i 8} {
            set d [expr {int(rand()*$dlim)}]
            lappend dels $d
            if {$d < 1} {set d 1}
            set bgfill [expr {$blackfill ? 0 : [lindex $bgs1 end]}]
            set bgs1 [concat \
                [lrange $bgs1 0 ${i}-1] \
                [lrange $bgs1 $i+$d end] \
                [lrepeat $d $bgfill]]
            set chs1 [concat \
                [lrange $chs1 0 ${i}-1] \
                [lrange $chs1 $i+$d end] \
                [lrepeat $d " "]]
        }

        # First line: desired appearance without using deletion
        append s $crlf
        foreach ch $chs1 bg $bgs1 {
            append s [format {%c[;%dm%s} 27 [expr {40+$bg}] $ch]
        }

        # Second line: uses deletion
        append s $crlf
        foreach ch $chs0 bg $bgs0 {
            append s [format {%c[;%dm%s} 27 [expr {40+$bg}] $ch]
        }
        append s "$e\[m"
        append s $cr
        foreach del $dels {
            append s [format {%c[%dP%c} 27 $del 9]
        }
    }

    # Sixth pair of lines: how attributes are filled in at the end of line
    append s $crlf
    append s $crlf
    set bgs [list]
    for {set i 0} {$i < 80} {incr i} {
        set bg [expr {40+int(rand()*6)}]
        lappend bgs $bg
        append s [format {%c[;%dm } 27 $bg]
    }
    append s $crlf
    for {set i 0} {$i < 80} {incr i} {
        append s [format {%c[;%dm@} 27 [expr {int(rand()*6)}]]
    }
    append s $cr
    foreach bg $bgs {
        append s [format {%c[;%dm%c[P} 27 $bg 27]
    }
    append s "${e}\[m$crlf"

    append s $crlf

    return $s
}

set patterns(chardel2) "A few more cases of character deletion"
# "chardel2": Shows a few things done with deletion.  Each line is
# supposed to be accompanied by a label explaining what it's doing.
# Or you can compare against another terminal.
proc pat_chardel2 {} {
    set e [format %c 27]
    set crlf [format %c%c 13 10]
    set cr [format %c 13]
    set t [format %c 9]
    set bs [format %c 8]

    append s $crlf$crlf
    append s "Odd digits:$crlf"
    append s "0123456789$cr"
    append s "${t}${e}\[P"
    foreach x {_ _ _ _} {
        append s "${bs}${bs}${e}\[P"
    }
    append s $crlf

    foreach dat {"" 7 777} {
        append s $crlf
        append s "Delete rightmost character only ($dat):$crlf"
        for {set i 33} {$i < 113} {incr i} {
            append s [format %c $i]
        }
        append s "${e}\[${dat}P${crlf}"
    }

    foreach {dats dat} {"" 1 0 1 1 1 7 7 769 769} {
        append s $crlf
        append s "Delete all characters ($dats):$crlf"
        for {set i 33} {$i < 113} {incr i} {
            append s [format %c $i]
        }
        append s $cr
        set have 80
        while {$have > 0} {
            append s "${e}\[${dats}P"
            set have [expr {$have - $dat}]
        }
        append s $crlf
    }
    
    return $s
}

set patterns(chardelfc) "Use slow character deletion to test XON/XOFF"
# "chardelfc" - Pairs of lines that should match.  Mainly this is a test
# of flow control, and functions based on the fact that my implementation
# of ^[[...P, which deletes characters in the current line, is inefficient.
proc pat_chardelfc {} {
    set count 8 ; # number of pairs of lines
    set s ""

    append s "\r\n\r\n"

    while {$count > 0} {
        # Count one more line pair.
        incr count -1

        # Generate pattern of vertical bars & blanks (& number to go)
        set lp [format {%d } $count]
        while {[string length $lp] < 80} {
            append lp [format {%s} [expr {(rand()<0.5)?"|":" "}]]
        }

        # Emit the line, the first time, without anything special happening.
        append s $lp
        append s "\r\n"

        # Emit the line, the second time, with character deletions.  And
        # some garbage to delete.
        append s [string repeat "*" 80]
        append s "\r"
        foreach c [split $lp ""] {
            append s [format {%c[80P} 27]
            append s $c
        }
        append s "\r\n"

        # Blank line to separate the pairs.
        append s "\r\n"
    }

    return $s
}

set patterns(junk) "Fill screen in with junk and scroll"
# "junk": My main reason for having this, is that it can sometimes alter the
# results of the test cases that come after, in case of a bug in display
# memory management in VTJ1.
proc pat_junk {} {
    set s ""
    set nc 5000
    for {set i 0} {$i < $nc} {incr i} {
        append s [format %c [expr {int(rand()*95+33)}]]
        if {$i == $nc-1 || rand()<0.2} {
            set atts [list ""]
            if {$i != ($nc-1)} {
                foreach a {1 4 7} {
                    if {rand()<0.5} { lappend atts $a }
                }
                foreach b {30 40} {
                    if {rand()<0.5} { lappend atts [expr {$b+int(rand()*8)}] }
                }
            }
            append s [format {%c[%sm} 27 [join $atts {;}]]
        }
    }
    append s [format %c%c 13 10]
    return $s
}

# several patterns for scrolling in scroll region
# "scrollrgn_*": Several patterns for scrolling in a scroll region.
# Can be compared with another terminal.
foreach {srp_lbl srp_slb srp_top srp_bot} {
    high h 0 9
    low l 16 0
    mid m 10 15
} {
    foreach {org_lbl org_slb org_val} {
        "not origin mode" n l
        "origin mode" o h
    } {
        set pn scrollrgn_${srp_slb}${org_slb}
        set patterns($pn) "Scrolling in scroll region, ${srp_lbl}, ${org_lbl}"
        eval [list proc "pat_$pn" [list] \
            [list patx_scrollrgn $srp_top $srp_bot $org_val]]
    }
}
proc patx_scrollrgn {top bot org} {
    set s ""
    # set or reset origin mode 
    append s [format {%c[?6%s} 27 $org]
    # set scroll region
    append s [format {%c[%d;%dr} 27 $top $bot]
    # fill screen with something, mostly just line markers
    set ebot [expr {$bot ? $bot : 40}]
    for {set i 0} {$i < $ebot + 3} {incr i} {
        if {!($i % 5)} {
            # make it double wide
            append s [format {%c#6} 27]
        }
        append s [format {%c[%d;%dm<>%c[m} \
            27 \
            [expr {30+($i % 8)}] \
            [expr {40+(($i % 8) ^ (1 + ($i % 7)))}] 27]
        append s [format {Alpha %02d.% 10s} $i ""]
        append s [format {Alpha %02d.% 10s} $i ""]
        append s "\r\n"
    }
    return $s
}

set patterns(modeflgs1) "Unrecognized mode flags test I"
# modeflgs1: Tests several mode flags, along with some others that
# are unrelated and might not be recognized.  Will print on the screen
# descriptions of what you should be seeing.  This one cannot be compared
# against another terminal since it uses some VTJ-1 private modes.
proc pat_modeflgs1 {} {
    set s ""
    # initial settings for recognized mode flags
    #       + error checkerboard on (not seen in xterm)
    #       + block cursor on (not seen in xterm)
    #       + screenwise reverse video off
    #       + origin mode off
    append s [format {%c[=1h%c[=3h%c[?5l%c[?6l} 27 27 27 27]
    # set colors
    append s [format {%c[;31;40m} 27]
    # clear screen & go to top row
    append s [format {%c[r%c[2J} 27 27]
    # set some bogus mode flags
    append s [format {%c[=99h%c[1l%c[?3l%c[?77l%c[6h} 27 27 27 27 27]
    # set a scroll region
    append s [format {%c[3;15r} 27]
    # display some indicative stuff
    append s "Top of screen, red on black, block cursor.\r\n"
    append s "And here're two checkerboards in quotes: \""
    append s [format {%c%c%c%c} 27 24 27 26]
    append s "\".\r\n"
    return $s
}

set patterns(modeflgs2) "Unrecognized mode flags test II"
# modeflgs2: Tests several mode flags, along with some others that
# are unrelated and might not be recognized.  Will print on the screen
# descriptions of what you should be seeing.  This one cannot be compared
# against another terminal since it uses some VTJ-1 private modes.
proc pat_modeflgs2 {} {
    set s ""
    # initial settings for recognized mode flags
    #       + error checkerboard off (not seen in xterm)
    #       + block cursor off (not seen in xterm)
    #       + screenwise reverse video on
    #       + origin mode on
    append s [format {%c[=1l%c[=3l%c[?5h%c[?6h} 27 27 27 27]
    # set colors
    append s [format {%c[;31;40m} 27]
    # clear screen & go to top row
    append s [format {%c[r%c[2J} 27 27]
    # set some bogus mode flags
    append s [format {%c[=99l%c[1h%c[?3h%c[?77h%c[6l} 27 27 27 27 27]
    # set a scroll region
    append s [format {%c[3;15r} 27]
    # display some indicative stuff
    append s "Third row, black on red, underline cursor.\r\n"
    append s "And here're two quotes: \""
    append s [format {%c%c%c%c} 27 24 27 26]
    append s "\".\r\n"
    return $s
}

set patterns(modeflgs3) "Unrecognized mode flags test III"
# modeflgs3: Tests several mode flags, along with some others that
# are unrelated and might not be recognized.  Will print on the screen
# descriptions of what you should be seeing.  This one cannot be compared
# against another terminal since it uses some VTJ-1 private modes.
proc pat_modeflgs3 {} {
    set s ""
    # initial settings for recognized mode flags
    #       + error checkerboard off (not seen in xterm)
    #       + block cursor on (not seen in xterm)
    #       + screenwise reverse video on
    #       + origin mode off
    append s [format {%c[=1l%c[=3h%c[?5h%c[?6l} 27 27 27 27]
    # set colors
    append s [format {%c[;31;40m} 27]
    # clear screen & go to top row
    append s [format {%c[r%c[2J} 27 27]
    # set some bogus mode flags
    append s [format {%c=257h%c[=259l%c[?517l%c[?774h} 27 27 27 27]
    append s [format {%c[=99l%c[1h%c[?3h%c[?77h%c[6l} 27 27 27 27 27]
    # set a scroll region
    append s [format {%c[3;15r} 27]
    # display some indicative stuff
    append s "Top row, black on red, block cursor.\r\n"
    append s "And here're two quotes: \""
    append s [format {%c%c%c%c} 27 24 27 26]
    append s "\".\r\n"
    return $s
}

set patterns(cumo1) "Cursor movements, in particular multiples"
# cumo1: Writes some text (white) and draws some characters in various
# positions over and around it (cyan).  For comparison with another terminal.
proc pat_cumo1 {} {
    set s ""
    append s [format {%c[r%c[2J} 27 27]
    append s "This is a test.\r\n"
    append s "This is only a test.\r\n"
    append s "What else would it be?\r\n"
    # for more visible text
    append s [format {%c[;36m} 27]
    # moving right with "CUF" code
    append s [format {*%c[3C*%c[C*%c[2C*%c[1C*%c[0C*} 27 27 27 27 27]
    # moving down with "CUD" code
    append s [format {#%c[3B#%c[B#%c[2B#%c[1B#%c[0B#} 27 27 27 27 27]
    # moving left with "CUB" code
    append s [format {$%c[6D$%c[D$%c[0D$%c[1D$%c[4D$} 27 27 27 27 27]
    # moving up with "CUU" code
    append s [format {@%c[3A@%c[A@%c[2A@%c[1A@%c[0A@} 27 27 27 27 27]
    # moving down a lot
    append s [format {%c[256BW} 27]
    # moving right a lot
    append s [format {%c[256CX} 27]
    # moving up a lot
    append s [format {%c[256AY} 27]
    # moving left a lot
    append s [format {%c[256DZ} 27]
    return $s
}

set patterns(cupos) "Cursor positioning with CUP and HVP"
# cupos: Draws a bunch of upper case letters more or less randomly around
# the screen, to test the cursor positioning escape sequences.
# For comparison against another terminal.  This is pseudorandom so
# you can use "srand $seed" to change it.
proc pat_cupos {} {
    set s ""
    append s [format {%c[H%c[J} 27 27] ; # clear screen
    foreach om {h l h l} { # we'll run with origin mode on & off
        append s [format {%c[?6%s} 27 $om]
        foreach sr {"" 5;15 20;30 15;25 "" 10;20} { # different scroll regions
            append s [format {%c[%sr} 27 $sr]
            for {set i 0} {$i < 20} {incr i} {
                # go to a position and display a character
                # the parameters given to H or f are usually one or two,
                # sometimes none; this script will occasionally go up to
                # ten.
                set parms [list]
                switch -- [expr {int(rand()*10)}] {
                    0 -
                    1 { set nparms [expr {int(rand()*7+3)}] }
                    2 { set nparms 0 }
                    3 -
                    4 { set nparms 1 }
                    default { set nparms 2 }
                }
                while {[llength $parms] < $nparms} {
                    # come up with a parameter
                    switch -- [expr {int(rand()*10)}] {
                        0 -
                        1 { set parm "" }
                        2 -
                        3 { set parm 0 }
                        4 -
                        5 -
                        6 -
                        7 { set parm [expr {int(rand()*80+1)}] }
                        8 -
                        9 { set parm [expr {int(rand()*1000+81)}] }
                    }
                    lappend parms $parm
                }
                append s [format {%c[%s%s} \
                    27 [join $parms ";"] [expr {(rand()<0.5)?"H":"f"}]]
                append s [format %c [expr {int(rand()*26)+65}]]
            }
        }
    }
    return $s
}

set patterns(tabstops) "Various tab stops, same text lines"
# tabstops: Draws the same text line (a mix of spaces and letters) five
# times, using different tab stop settings.  The five times should look
# identical.  You can also compare against another terminal.  This is
# pseudorandom, so "srand $seed" will change it up.
proc pat_tabstops {} {
    set s ""
    append s "\r\n"

    # generate a line of text with a lot of gaps
    set line ""
    set charmode 0
    while {[string length $line] < 80} {
        if {rand() < 0.2} { set charmode [expr {!$charmode}] }
        if {$charmode} {
            # character
            append line [format %c [expr {65+int(rand()*26)+int(rand()*2)*32}]]
        } else {
            # blank space
            set n1 [expr {80 - [string length $line]}]
            set n2 [expr {int(rand()*6+1)}]
            append line [string repeat " " [expr {min($n1,$n2)}]]
        }
    }

    # Write the line five times with different tab stops
    set tabstats [list]
    foreach time {notab deftab defchg new newchg quit} {
        # Ruled lines above & below
        if {$time eq "notab" || $time eq "quit"} {
            for {set i 0} {$i < 80} {incr i 5} {
                append s $i
                append s [string repeat " " [expr {5-[string length $i]}]]
            }
            append s "\r\n"
        }

        # Change the tab stops as desired
        switch $time {
            notab {
                # Start out pretending there are no tab stops
                for {set i 0} {$i < 80} {incr i} {
                    set stops($i) 0
                }
            }
            deftab {
                # Assume the default tab stops
                foreach i {8 16 24 32 40 48 56 64 72 79} {
                    set stops($i) 1
                }
            }
            newchg -
            defchg {
                # Change a few of the currently set tab stops.
                for {set i 0} {$i < 79} {incr i} {
                    if {$stops($i)} {
                        if {rand() < 0.5} {
                            # Clear this one.
                            append s "\033\[g"
                            set stops($i) 0
                        }
                    } else {
                        if {rand() < 0.125} {
                            # Set this one.
                            append s "\033H"
                            set stops($i) 1
                        }
                    }
                    append s " "
                }

                # Return to left edge
                append s "\r"
            }
            new {
                # Come up with some completely new tab stops
                for {set i 0} {$i < 79} {incr i} {
                    set stops($i) 0
                }
                set stops(79) 1
                for {set i 0} {$i < 10} {incr i} {
                    set stops([expr {int(rand()*79)}]) 1
                }

                # Clear existing tab stops
                append s "\033\[3g"

                # Set the new tab stops
                for {set i 0} {$i < 80} {incr i} {
                    if {$stops($i)} {
                        append s "\033H"
                    }
                    append s " "
                }

                # Return to left edge
                append s "\r"
            }
            quit {
                # Back to default tab stops for next time, but don't
                # bother recording them in $stops(...)
                append s "\033\[3g"
                for {set i 0} {$i < 80} {incr i} {
                    if {$i == 79 || !($i % 8)} {
                        append s "\033H"
                    }
                    append s " "
                }

                # Return to left edge
                append s "\r"
            }
        }

        if {$time eq "quit"} break

        # Write the line
        set tabstat 0
        for {set p 0} {$p < 80} {} {
            if {[string index $line $p] eq " "} {
                # It's space.  Is there a tab stop I can use?
                set tabbed 0
                for {set q $p} {[string index $line $q] eq " "} {incr q} {
                    if {$q > $p && $stops($q)} {
                        # Yes.
                        append s "\t"
                        set p $q
                        set tabbed 1
                        incr tabstat
                        break
                    }
                }
                if {!$tabbed} {
                    append s " "
                    incr p
                }
            } else {
                # It's not space; emit it.
                append s [string index $line $p]
                incr p
            }
        }
        append s "\r\n"
        lappend tabstats $tabstat
    }

    append s [format "Tab stats: %s\r\n" $tabstats]

    return $s
}

set patterns(rightedge) "Various things happening on right edge of screen"
# "rightedge": Draws various things along the edges of the screen, and
# various colored stripes.  The most interesting thing is what happens
# on the right edge of the screen.  Compare to another terminal.
proc pat_rightedge {} {
    set s "\r\n"

    set clrctr 0
    foreach {hop op} {
        nil "" lf "\n" cr "\r" crlf "\n\r"
        sp " " tab "\t" bs "\010" njl ""
        up "\033\[A" dn "\033\[B" lft "\033\[D" rgt "\033\[C"
    } {
        append s "\r\n"
        append s [format {%c[;%d;%dm} \
            27 [expr {40+($clrctr&7)}] \
            [expr {30+((~$clrctr)&7)}]]
        incr clrctr
        append s "<$hop>"
        append s [string repeat " " [expr {77 - [string length $hop]}]]
        append s $op
        append s $hop
    }

    append s "\r\n"
    append s [format {%c[m} 27]

    return $s
}

set patterns(rightel) "\"EL\" sequence at right edge"
# "rightel": Tests what the "EL" sequence does at the right edge of the
# screen.  Compare with another terminal.
proc pat_rightel {} {
    set crlf "\r\n"
    set s $crlf
    append s [format {%s%c[Ks%s} [string repeat a 80] 27 $crlf]
    append s [format {%s%c[Ks%s} [string repeat " " 80] 27 $crlf]
    append s [format {%s%c[Ks%s} [string repeat "\t" 10] 27 $crlf]
    append s $crlf
    return $s
}

set patterns(bgstripes) "Horizontal stripes made via background color"
# "bgstripes": Draws stripes of various lengths, using different background
# colors.  For comparison against another terminal.  Pseudorandom, so
# "srand $seed" will change its output.
proc pat_bgstripes {} {
    set s ""

    # clear screen
    append s [format {%c[m%c[H%c[2J} 27 27 27]

    # make stripes, enough to scroll a little
    for {set i 0} {$i < 55} {incr i} {
        append s [format {%c[%dm} 27 [expr {41+($i % 7)}]]
        append s [string repeat " " [expr {4*int(1+rand()*5)}]]
        append s "\r\n"
    }

    append s [format {%c[m} 27]

    return $s
}

set patterns(eraseinline) "'erase in line' sequence"
# "eraseinline": Tests erasing part of a line (left of the cursor, right
# of the cursor, or the whole line) and what gets put in its place.
# For comparison against another terminal.
proc pat_eraseinline {} {
    set s ""
    # Clear character attributes and blank a line
    append s "\033\[m\r\n\r\n"
    # Different variations of the ^[[K sequence.
    set ctr 0
    foreach rg {"" x 1 x 2} {
        foreach ig {"" 6 3 4} {
            incr ctr

            # row attribute if any
            if {$ig ne ""} {
                append s "\033#$ig"
            }
            # initial text
            for {set c 0} {$c < 80} {incr c} {
                append s [format %c [expr {97+($c % 26)}]]
            }
            # now, go to the position where we do the blanking
            append s "\r\t\t"
            # and set the colors we'll use for blanking
            append s [format {%c[;%d;%dm} \
                27 [expr {31+($ctr % 7)}] [expr {40+($ctr % 7)}]]
            # and do the blanking
            if {$rg eq "x"} {
                append s [format {%c[%sK} 27 [expr {3<<($ctr >> 1)}]]
            } else {
                append s [format {%c[%sK} 27 $rg]
            }
            # and clear the colors
            append s "\033\[m"
            # and draw a character if whole row is blanked
            if {$rg eq 2} {
                append s "\r\tA"
            }
            # and now on to the next row
            append s "\r\n"
        }
    }

    return $s
}

set patterns(insdelline) "Insert/delete line sequences"
# "insdelline": Inserts and deletes whole lines on the screen, with various
# background colors, and occasionally writes a few characters of text in them.
# For comparison against other terminals.  Pseudorandom, so you get a
# slightly different test with different values of "srand $seed".
proc pat_insdelline {} {
    global height
    set s ""

    append s "\033\[H\033\[2J"

    set ltr 0
    for {set i 0} {$i < 100} {incr i} {
        # maybe set scroll region
        if {rand() < 0.1} {
            set rows [list]
            foreach dummy {0 1} {
                lappend rows [expr {int(rand()*$height+1)}]
            }
            set rows [lsort -integer $rows]
            if {rand() < 0.4} {
                set rows [list]
            }
            append s [format {%c[%sr} 27 [join $rows "\;"]]
        }

        # set foreground & background color
        append s [format {%c[%d;%dm} \
            27 [expr {40+($i % 8)}] [expr {37-($i % 8)}]]

        # maybe set cursor position
        if {rand() < 0.5} {
            append s [format {%c[%d;%dH} 27 \
                [expr {int(rand()*$height+1)}] 1]
        }

        # figure out parameter(s) for insertion/deletion
        set parms [list]
        set scale 0
        foreach prob {0.7 0.4 0.1} {
            set scale0 0
            if {rand() < $prob} {
                if {rand() < 0.1} {
                    set parm ""
                    set scale0 1
                } elseif {rand() < 0.1} {
                    set parm [expr {int($height*((1.25+rand())**4))}]
                    set scale0 $parm
                } else {
                    set parm [expr {int((rand()**3)*$height*1.25)}]
                    set scale0 $height
                }
                lappend parms $parm
            }
            if {$scale == 0} { set scale $scale0 }
        }

        # do insertion/deletion
        append s [format {%c[%s%s} 27 \
            [join $parms "\;"] [expr {(rand()<0.5) ? "L" : "M"}]]

        # since there are problems with XON/XOFF, and the above is a slow
        # escape sequence, inserting a delay
        append s [format {%c[==%d|} 27 [expr {$scale * 1}]]

        # since there's a difference between xterm & VTJ-1, in that the former
        # moves the cursor to the left during insertion/deletion:
        # move the cursor to the left
        append s [format {%c[99D} 27]

        # put some text
        for {set j -1} {$j < rand() * 10} {incr j} {
            append s [format %c [expr {($ltr % 26)+97}]]
            incr ltr
        }

        # maybe change line attributes (normal & double width only; double
        # height is inconvenient to compare results)
        if {rand() < 0.4} {
            append s [format {%c#%c} 27 [expr {53+int(rand()*2)}]]
        }
    }

    # set things back the way they were
    append s [format {%c[m%c[r} 27 27]

    return $s
}

set patterns(index) "ESC-D (IND) or ESC-M (RI) to move and maybe scroll"
# "index": Draws letters, upper and lower case, in slanted lines across
# the screen. Also some numbers toward the right edge, and one colored
# stripe across.  Tests where scroll happens and
# where it doesn't, and where line wrap is not supposed to be happening.
# Compare against another terminal.
proc pat_index {} {
    # Takes parameter: "parm 0" (IND), "parm 1" (RI), or "parm 2" (LF)
    global parameter
    set s ""

    # decide what sequence to use to move
    if {$parameter eq "0"} {
        # use IND to move down
        set seq "\033D"
        set rfirst 1
        set rlast 40
        set rstep 1
    } elseif {$parameter eq "1"} {
        # use RI to move up
        set seq "\033M"
        set rfirst 40
        set rlast 1
        set rstep -1
    } elseif {$parameter eq "2"} {
        # use LF character to move down
        set seq "\012"
        append s "\033\[20l" ; # disable linefeed/new line mode
        exec stty -onlcr
        set rfirst 1
        set rlast 40
        set rstep 1
    } else {
        error "parameter value must be 0, 1, or 2"
    }

    # clear screen & set scroll region 8-28
    append s "\033\[m\033\[H\033\[2J\033\[8;28r"

    # now issue the IND sequence at various places making a diagonal
    # down the screen
    for {set i $rfirst} {$i != ($rlast + $rstep)} {incr i $rstep} {
        append s [format {%c[%d;%dH%c%c[%dm%s%c[m%c} \
            27 $i [expr {($i * 7) % 71}] \
            [expr {65+($i%26)+int($i/26)*32}] \
            27 [expr {41+($i%6)}] $seq 27 \
            [expr {97+($i%26)-int($i/26)*32}]]
    }

    # and on/near the right edge of the screen
    for {set i 12} {$i < 24} {incr i 4} {
        append s [format {%c[%d;78H1%s2%s3%s4} \
            27 $i $seq $seq $seq]
    }

    return $s
}

set patterns(nelesc) "ESC-E (NEL) to move and maybe scroll"
# "nelesc": To test an escape sequence that moves the cursor and maybe
# scrolls; compare against another terminal.  Numbers on the left edge
# of the screen, twos ets; numbers diagonally across the middle of
# the screen; a couple of asterisks on the left and right edges of the
# screen, and a purple stripe.
proc pat_nelesc {} {
    # Takes parameter: "parm 0" (NEL) or "parm 1" (CRLF)
    global parameter
    set s ""

    if {$parameter eq "0"} {
        # use NEL to move down
        set seq "\033E"
    } elseif {$parameter eq "1"} {
        # use CRLF to move down
        set seq "\015\012"
        append s "\033\[20l" ; # disable linefeed/new line mode
        exec stty -onlcr
    } else {
        error "parameter value must be 0 or 1"
    }

    # clear screen & set scroll region 8-28
    append s "\033\[m\033\[H\033\[2J\033\[8;28r"

    # do the same thing in different positions
    for {set i 40} {$i >= 1} {incr i -1} {
        set l1 "($i)"
        set l2 [string repeat "<$i>" [expr {2 - ($i % 2)}]]
        append s [format {%c[%d;%dH%s%c[%dm%s%c[m%s} \
            27 $i [expr {20 + (($i * 3) % 41)}] \
            $l1 27 [expr {41+($i%6)}] $seq 27 $l2]
    }

    # try the right edge of the screen
    append s [format {%c[20;80H*%s*} 27 $seq]

    return $s
}

set patterns(lfnl) "linefeed / newline mode (needs nonlcr)"
# "lfnl": Tests line feed / newline mode.  Specify the "nonlcr" option or
# it won't work right.  For comparison against another terminal.
# Pseudorandom, so "srand $seed" will give you a slightly different test.
proc pat_lfnl {} {
    set s ""
    append s "\033\[H\033\[2J\033\[5;25r"

    for {set i 0} {$i < 60} {incr i} {
        append s [format {%c[20%s%c%c%c%c} \
            27 [expr {(rand() < 0.5) ? "l" : "h"}] \
            [expr {97+(($i*3)%26)}] \
            [expr {97+(($i*3+1)%26)}] \
            [expr {97+(($i*3+2)%26)}] \
            [expr {10+int(rand()*3)}]]
    }

    append s "\033\[r\033\[20l"
}

set patterns(csr2) "cursor save & restore: position & char attributes"
# "csr2": Tests save/restore of cursor property, by writing the alphabet
# in several lines, each with different attributes.  Compare the lines
# against each other.  Pseudorandom, so "srand $seed" will give you a
# slightly different test.
proc pat_csr2 {} {
    set s ""
    set e "\033"
    append s "$e\[H$e\[2J$e\[m"
    set saved ""
    set cur ""
    set atts {
        0 4 1 7
        31 32 33 34 35 "36;41" 37 "36;40" 41 42 43 44 45 "30;46" "30;47"
    }
    set natts [llength $atts]
    set togo() 0
    for {set i 0} {$i < $natts} {incr i} {
        set pos($i) 1
        set togo($i) 26
        incr togo() 26
    }
    while {$togo() > 0} {
        # pick a place to write a character
        if {$saved ne "" && rand() < 0.6 && $togo($saved) > 0} {
            # restore
            append s "${e}8"
            set cur $saved
        } elseif {$cur ne "" && rand() < 0.4 && $togo($cur) > 0} {
            # just keep going
        } else {
            # position somewhere with some attributes
            while {1} {
                set i [expr {int(rand()*$natts)}]
                set att [lindex $atts $i]
                set row [expr {$i + 1}]
                set col $pos($i)
                if {$togo($i) > 0} break
            }
            append s "$e\[$row;${col}H$e\[;${att}m"
            set cur $i
        }
        # write a character
        append s [format %c [expr {96 + $pos($cur)}]]
        incr pos($cur)
        incr togo($cur) -1
        incr togo() -1
        # save cursor
        append s "${e}7"
        set saved $cur
    }
    append s "$e\[m${e}7"
}

set patterns(csr3) "cursor save & restore: position & character set"
# "csr3": Draws the lower case alphabet & the graphic character set on
# each of several lines.  Compare the lines to one another.  Pseudorandom,
# so "srand $seed" will give you a slightly different test.
proc pat_csr3 {} {
    set s ""
    set e "\033"
    append s "$e\[H$e\[2J$e\[m"

    # Initialize character set selection state
    set cs() 0 ; # selected: G0
    set cs(0) 0 ; # G0: US
    set cs(1) 1 ; # G1: GFX
    append s "${e}(B${e})0\017" ; # terminal codes for same

    # And positioning
    set cx 5
    set cy 5
    append s "${e}\[$cy;${cx}H"

    # Initialize our record of what needs to be written
    set cnt 0
    for {set y 1} {$y <= 10} {incr y} {
        for {set x 1} {$x <= 31} {incr x} {
            set ch($y,$x) [format %c [expr {95+$x}]]
            set cs($y,$x) 0
            incr cnt
        }
        for {set x 32} {$x <= 62} {incr x} {
            set ch($y,$x) [format %c [expr {64+$x}]]
            set cs($y,$x) 1
            incr cnt
        }
    }

    # And save state
    append s "${e}7"
    set ss() $cs()
    set ss(0) $cs(0)
    set ss(1) $cs(1)
    set sx $cx
    set sy $cy

    # And write, in pseudorandom order.
    while {$cnt > 0} {
        switch [expr {int(rand()*12)}] {
            0 {
                # Restore
                append s "${e}8"
                set cs() $ss()
                set cs(0) $ss(0)
                set cs(1) $ss(1)
                set cx $sx
                set cy $sy
            }
            1 {
                # Save
                append s "${e}7"
                set ss() $cs()
                set ss(0) $cs(0)
                set ss(1) $cs(1)
                set sx $cx
                set sy $cy
            }
            2 -
            3 {
                # Move randomly
                set ty [expr {int(rand()*10+1)}]
                set tx [expr {int(rand()*62+1)}]
                if {[info exists ch($ty,$tx)]} {
                    append s "${e}\[$ty;${tx}H"
                    set cx $tx
                    set cy $ty
                }
            }
            4 -
            5 {
                # Change charset selection randomly
                if {$cs()} {
                    set cs() 0
                    append s "\017"
                } else {
                    set cs() 1
                    append s "\016"
                }
            }
            6 {
                # Change charset G0 randomly
                if {$cs(0)} {
                    set cs(0) 0
                    append s "${e}(B"
                } else {
                    set cs(0) 1
                    append s "${e}(0"
                }
            }
            7 {
                # Change charset G1 randomly
                if {$cs(1)} {
                    set cs(1) 0
                    append s "${e})B"
                } else {
                    set cs(1) 1
                    append s "${e})0"
                }
            }
            8 -
            9 -
            10 -
            11 {
                # Write character if in the right state for it.
                if {
                    [info exists ch($cy,$cx)] &&
                    $cs($cy,$cx) == $cs($cs())
                } {
                    append s $ch($cy,$cx)
                    unset ch($cy,$cx)
                    incr cx
                    incr cnt -1
                }
            }
        }
    }

    append s "\017${e}(B${e})B"

    return $s
}

set patterns(csr4) "cursor save & restore: position & character set (II)"
# "csr4": Draws the lower case alphabet & the graphic character set on
# each of several lines.  Compare the lines to one another.  Pseudorandom,
# so "srand $seed" will give you a slightly different test.
proc pat_csr4 {} {
    set s ""
    set e "\033"
    append s "$e\[H$e\[2J$e\[m"

    # This one is less random.
    # Rows 2-9 are still done randomly, interspersed with rows 1-10
    # which are done in order, using save/restore state.

    # List of operations for rows 1+10
    set opo [list]
    lappend opo "${e}(B${e})0"
    foreach y {1 10} {
        lappend opo "${e}\[${y}H"
        foreach csc {"\017" "\016"} {
            lappend opo $csc
            for {set x 1} {$x <= 31} {incr x} {
                lappend opo [format %c [expr {95+$x}]]
            }
        }
    }

    # List of operations for rows 2-9
    set opx [list]
    for {set y 2} {$y <= 9} {incr y} {
        for {set x 1} {$x <= 62} {incr x} {
            set opx0 "${e}\[$y;${x}H"
            if {rand() < 0.5} {
                if {$x <= 31} {
                    append opx0 "${e}(B\017"
                } else {
                    append opx0 "${e}(0\017"
                }
            } else {
                if {$x <= 31} {
                    append opx0 "${e})B\016"
                } else {
                    append opx0 "${e})0\016"
                }
            }
            append opx0 [format %c [expr {96+(($x-1)%31)}]]
            lappend opx [list [expr {rand()}] $opx0]
        }
    }

    # Randomize the operation list
    set opy [lsort -index 0 -real $opx]

    # Extract just the operations from that.
    set opr [list]
    foreach o $opy {
        lappend opr [lindex $o 1]
    }

    # Now, issue operations, a few at a time from each list.  When
    # switching lists issue ^[[7 or ^[[8 to save/restore
    while {1} {
        # count the operations in the lists
        set no [llength $opo]
        set nr [llength $opr]
        set nt [expr {$no + $nr}]
        if {!$nt} break ; # done
        ##puts -nonewline stderr "(nt=$nt)"

        # pick one of the lists, randomly, weighted by length
        # and take a random small number of operations off it
        if {(rand()*$nt) < $no} {
            # take from the "ordered" operations
            set ng [expr {min($no,int(rand()*5+1))}]
            set ops [lrange $opo 0 ${ng}-1]
            set opo [lrange $opo $ng end]
            set pfx "${e}8"
            set sfx "${e}7"
        } else {
            # take from the "randomized" operations
            set ng [expr {min($nr,int(rand()*5+1))}]
            set ops [lrange $opr 0 ${ng}-1]
            set opr [lrange $opr $ng end]
            set pfx ""
            set sfx ""
        }
        if {$ng < 1} { error "ng=$ng no=$no nr=$nr" }

        # perform the operations
        append s $pfx
        foreach o $ops {
            append s $o
        }
        append s $sfx
    }

    append s "\017${e}(B${e})B"

    return $s
}

set patterns(csr5) "cursor save & restore: position after scroll"
# "csr5": Draws horizontal colored stripes and a column of upper case letters.
# Compare to another terminal.  Pseudorandom, so "srand $seed" will give you
# a slightly different test case.
proc pat_csr5 {} {
    global parameter height
    set s ""
    set e "\033"
    append s "$e\[H$e\[2J$e\[m"

    # Use ^[D and ^[M to scroll; then restore cursor position and
    # draw a character.  Depends on parameter.

    # Pick position and save
    append s [format {%c[%d;%dH} 27 \
        [expr {int(rand()*10+10)}] \
        [expr {int(rand()*72+1)}]]
    append s "${e}7"

    # Draw character, scroll, repeat.
    for {set i 65} {$i <= 90} {incr i} {
        append s [format %c $i]
        if {$parameter eq "0"} {
            # move to bottom
            append s [format {%c[%dH} 27 $height]
            # set background color
            append s [format {%c[%dm} 27 [expr {41+($i%6)}]]
            # index (down, forcing scroll)
            append s "${e}D"
        } else {
            # move to top
            append s [format {%c[H} 27]
            # set background color
            append s [format {%c[%dm} 27 [expr {41+($i%6)}]]
            # reverse index (up, forcing scroll)
            append s "${e}M"
        }
        # Restore
        append s "${e}8"
    }

    return $s
}

set patterns(csr6) "Cursor save & restore: right edge of screen"
# "csr6": Draws letters, in various colors, around the right edge of the
# screen.  Compare to another terminal.  Pseudorandom, so "srand $seed" will
# Compare to another terminal.
proc pat_csr6 {} {
    set width 80
    set s ""
    set e "\033"
    set crlf "\015\012"

    append s "$e\[H$e\[2J$e\[m"

    # In each of several rows: go to a few columns left of the edge,
    # draw one or more characters, save or restore, draw characters to the
    # right edge.
    set charctr 0
    foreach {row nc1 rst} {
        1  1 0
        2  2 1
        3  4 0
        4  5 1
        5  2 0
        6  3 1
        7  5 0
        8  1 1
        9  3 0
        10 4 1
    } {
        # foreground color specific to this row
        append s "$e\[;[expr {31+($row%7)}]m"
        # go to the proper position
        append s "$e\[${row};[expr {$width-4}]H"
        # characters
        for {set i 0} {$i < $nc1} {incr i} {
            incr charctr
            append s [format %c [expr {97 + ($charctr % 26)}]]
        }
        # save or restore
        if {$rst} {
            append s "${e}8"
        } else {
            append s "${e}7"
        }
        # character
        incr charctr
        append s [format %c [expr {97 + ($charctr % 26)}]]
    }

    append s "$e\[m"
    return $s
}

set patterns(csr7) "Cursor save & restore: erase-in-line bg after restore"
# "csr7": Draws lines of 'a' and of solid blocks, in various colors, with
# a sort of diagonal split.  For comparison to another terminal.
proc pat_csr7 {} {
    set width 80
    set s ""
    set e "\033"
    set crlf "\015\012"

    append s "$e\[H$e\[2J$e\[m"

    # This will issue the erase in line sequences (^[[0K through ^[[2K)
    # with various color attribute settings, after restoring cursor state.

    # First, fill in the rows with text.
    for {set row 1} {$row <= 18} {incr row} {
        # position in that row
        append s "$e\[${row}H"
        # foreground color, black background
        append s "$e\[;[expr {31+($row%7)}];40m"
        # text filling the row
        append s [string repeat a $width]
    }

    # Next, do the erases
    set 356 [list 3 5 6]
    for {set row 1} {$row <= 18} {incr row} {
        # go to the row, to the middle of it
        append s "$e\[${row};[expr {3+$row}]H"
        # set up some character attributes
        set bg1 [expr {1 + ($row % 7)}]
        set fg1 [expr {$bg1 ^ [lindex ${356} [expr {$row % 3}]]}]
        append s "$e\[;[expr {40+$bg1}];[expr {30+$fg1}]"
        if {$row % 2} {
            append s ";7"
        }
        append s "m"
        # save cursor
        append s "${e}7"
        # set different character attributes
        set bg2 [expr {1 + (($row * 5) % 7)}]
        set fg2 [expr {$bg2 ^ [lindex ${356} [expr {($row * 5) % 3}]]}]
        append s "$e\[;[expr {40+$bg2}];[expr {30+$fg2}]"
        if {($row % 4) < 2} {
            append s ";7"
        }
        append s "m"
        # restore cursor
        append s "${e}8"
        # do one of the three erase in line sequences
        switch [expr {$row % 4}] {
            0 { append s "$e\[K" }
            1 { append s "$e\[1K" }
            2 { append s "$e\[2K" }
            3 { append s "$e\[0K" }
        }
    }

    append s "$e\[m"
    return $s 
}

set patterns(csr8) "Cursor save & restore: mode flags"
# "csr8": This will pick some pseudorandom mode flags and test them with
# cursor save & restore.  Will also print out text explaining what to
# expect to see.  Some of the things that it does, you won't be able to see
# whether it's on without doing additional steps manually afterwards
# (for instance, ^G tests the visible & audible bell settings; typing at the
# keyboard tests the local echo and keyboard lock.)  Since it's
# pseudorandom you can use the "srand $seed" option.
# Not very good for comparison against another terminal for various reasons:
# use of VTJ-1 private mode settings; use of some tricks that don't seem to
# work on all terminals; and differences in how the cursor save & restore
# feature works on different terminals.  Oh, and some combinations of mode
# settings make the others hard to see.
proc pat_csr8 {} {
    global parameter
    set width 80
    set s ""
    set e "\033"
    append s "$e\[H$e\[2J$e\[m$e\[r"

    # Decide on mode settings; decide each one twice so that we can
    # do restore.
    set mode_labels [list]
    set mode_sets_1 [list]
    set mode_sets_2 [list]
    set mode_order() 0
    set info {
        "Checkerboard for invalid control chars" "\[=1" 0
        "Lock keyboard" "\[2" 0
        "Block cursor" "\[=3" 0
        "Visual bell" "\[=4" 0
        "Audible bell" "\[=5" 0
        "No local echo" "\[12" 0
        "Origin mode" "\[?6" 1
        "Linefeed/New Line Mode" "\[20" 0
        "Auto line wrap" "\[?7" 0
        "Insert mode" "\[4" 0
    }
    foreach {lbl seq swp} $info {
        set v0 [expr {int(rand()*2)}]
        set v1 [expr {int(rand()*2)}]
        lappend mode_labels "${lbl}: [expr {($swp ? $v0 : $v1) ? "on" : "off"}]"
        lappend mode_sets_1 $e$seq[expr {$v0 ? "h" : "l"}]
        lappend mode_sets_2 $e$seq[expr {$v1 ? "h" : "l"}]
        set mode_order($mode_order()) $mode_order()
        incr mode_order()
    }

    # Pseudorandomly order them
    for {set i 0} {$i < 100} {incr i} {
        set j [expr {1 + int(rand()*($mode_order() - 1))}]
        set mo $mode_order(0)
        set mode_order(0) $mode_order($j)
        set mode_order($j) $mo
    }

    # Display them
    for {set i 0} {$i < $mode_order()} {incr i} {
        set j $mode_order($i)
        append s "$e\[[expr {$j+1}]H"
        append s [lindex $mode_labels $j]
    }

    # and set them the first time around
    for {set i 0} {$i < $mode_order()} {incr i} {
        set j $mode_order($i)
        append s "$e\[[expr {$j+1}]H"
        append s [lindex $mode_sets_1 $j]
    }

    # Save cursor, change them, and restore cursor
    append s "${e}7"
    for {set i 0} {$i < $mode_order()} {incr i} {
        set j $mode_order($i)
        append s [lindex $mode_sets_2 $j]
    }
    append s "${e}8"

    # Move cursor to below them
    set p [expr {$mode_order()+2}]
    append s "$e\[${p}H"

    # Origin mode can be almost automatically determined
    incr p
    append s "$e\[${p}H"
    set s1 "Origin mode seems to be: "
    append s $s1
    set x [expr {[string length $s1] + 1}]
    append s "$e\[2r$e\[[expr {${p}-1}];${x}Hon"
    append s       "$e\[${p};${x}Hoff"
    append s "$e\[r$e\[[expr {${p}-1}];${x}H  "
    append s       "$e\[[expr {${p}+1}];${x}H   "

    # Linefeed/newline mode can too
    incr p 2
    append s "$e\[${p}H"
    set s1 "   Linefeed/New Line Mode seems to be: "
    append s $s1
    set x [expr {[string length $s1] + 1}]
    append s "on$e\[2D\n$e\[Aoff$e\[${p}H   "

    # And auto wrap mode
    incr p 2
    append s "$e\[${p}H"
    set s1 "Auto wrap mode seems to be: "
    append s [format {%son%s} $s1 "\r\n"]
    append s [format {%*soff} [string length $s1] ""]
    append s [format {%c[%d;%dH...%c[%dD%c[%dC---} \
        27 $p $width 27 $width 27 [string length $s1]]

    if {$parameter & 1} {
        # the auto wrap auto detection shows up badly in many terminals
        # so showing something more literal
        incr p 2
        append s "$e\[${p}H"
        for {set i 0} {$i < 104} {incr i} {
            append s [format %c [expr {97+($i % 26)}]]
        }
    }

    # And insert mode (included only optionally because it messes
    # with the autodetection of other flags and also with the shell
    # if there is one)
    incr p 2
    append s "$e\[${p}H"
    set s1 "Insert mode seems to be: "
    append s "${s1}on off"
    append s [format {%c[%d;%dH...} 27 $p [expr {1+[string length $s1]}]]
    append s [format {%c[%d;%dH%c[3P} 27 $p [expr {1+[string length $s1]}] 27]
    append s [format {%c[%d;%dH%c[3P} 27 $p [expr {4+[string length $s1]}] 27]

    return $s
}

set patterns(calign) "Character alignment test (needs writeable_rom & devel_escapes; needs parm)"
# "calign": Tests the alignment of characters and their associated
# attributes.  In case that got messed up by settings related to the
# clock rate.  This has some special requirements/impacts:
#       + It can't be run on other terminals, only on VTJ-1.
#       + It replaces the '@' and '`' characters in the font, so you it
#       will interfere with normal use of VTJ-1.
#       + It needs a parameter.  Use "parm ###" where the value is:
#           - number of pixel rows per character in the font (10, 12, or 16)
#           - plus 32 if VTJ-1 was built with font_256_chars=1
#           - plus 64 if you want to use double width mode
#       + It needs VTJ-1 to have been built with the following optional
#       features:
#           devel_escapes=1 and writeable_font=1
proc pat_calign {} {
    global parameter

    set charhgt [expr {$parameter & 31}]
    set eightbit [expr {($parameter >> 5) & 1}]
    set double [expr {($parameter >> 6) & 1}]
    set fontbase 40960
    set fontstride [expr {$eightbit ? 256 : 128}]
    set char1 64 ; # replace character '@' with our first special one
    set char2 96 ; # replace character '`' with our first special one

    set s "\015\012\015\012"

    # Generate our special test character
    for {set i 0} {$i < $charhgt} {incr i} {
        append s [format {%c[92;%u;%u|} \
            27 \
            [expr {$fontbase + $fontstride * $i + $char1}] \
            [expr {(($i*2)<$charhgt) ? 0xf0 : 0x0f}]]
        append s [format {%c[92;%u;%u|} \
            27 \
            [expr {$fontbase + $fontstride * $i + $char2}] \
            [expr {(($i*2)<$charhgt) ? 0x0f : 0xf0}]]
    }

    # Use it to generate a block of yellow/black.  The block is as follows:
    #   + an innermost "dense" block, 8x8 char cells, 16x16 color cells
    #   + around that, "sparse" blocks, normal and offset depending
    #   on the side, 2 char cells wide
    for {set r 0} {$r < 12} {incr r} {
        for {set d 0} {$d <= $double} {incr d} {
            if {$double} {
                # double width, double height, lower or upper half
                append s [format {%c#%s} 27 [expr {$d ? "4" : "3"}]]
            }
            if {$r < 2} {
                # upper left corner: empty
                append s [format {%c[m  } 27]
                # upper border: non offset sparse blocks
                for {set c 2} {$c < 10} {incr c} {
                    append s [format {%c[%um } 27 [expr {(($r+$c)&1)?43:40}]]
                }
                # upper right corner: empty
                append s [format {%c[m  } 27]
            } elseif {$r < 10} {
                # left border: offset sparse blocks
                for {set c 0} {$c < 2} {incr c} {
                    append s [format {%c[%u;%um%c} 27 \
                        33 \
                        40 \
                        [expr {(($r+$c)&1) ? $char1 : $char2}]]
                }
                # middle: dense blocks
                for {set c 2} {$c < 10} {incr c} {
                    append s [format {%c[%u;%um%c} 27 33 40 $char1]
                }
                # right border: non offset sparse blocks
                for {set c 10} {$c < 12} {incr c} {
                    append s [format {%c[%um } 27 [expr {(($r+$c)&1)?43:40}]]
                }
            } else {
                # lower left corner: empty
                append s [format {%c[m  } 27]
                # left border: offset sparse blocks
                for {set c 2} {$c < 10} {incr c} {
                    append s [format {%c[%u;%um%c} 27 \
                        33 \
                        40 \
                        [expr {(($r+$c)&1) ? $char1 : $char2}]]
                }
                # lower right corner: empty
                append s [format {%c[m  } 27]
            }
            append s [format {%c[m%c%c} 27 13 10]
        }
    }

    return $s
}

set patterns(awrap) "Auto wrap mode on & off"
# "awrap": Tests the auto wrap mode flag, by writing near the right
# edge of the screen with it on or off.  Including changing it near the
# right edge of the screen.  Can be compared against other terminals,
# though note that many of them (including xterm & Mac OS X Terminal)
# do something differently.  Try the Linux or NetBSD consoles.
proc pat_awrap {} {
    set s ""
    set width 80

    # clear screen
    append s "\033\[H\033\[2J\033\[m"

    # write various lines or pairs thereof
    set l 0
    foreach {linenum f_before f_setat f_setafter} {
        0  h "" ""
        1  l "h" ""
        2  h "l" ""
        3  l "h" "l"
        4  h "l" "h"
        5  l "" ""
        6  h "h" ""
        7  l "l" ""
        8  h "h" "l"
        9  l "l" "h"
    } {
        # position the cursor in column 71
        append s [format {%c[%dC} 27 [expr {$width - 10}]]
        # set the flag
        append s [format {%c[?7%s} 27 $f_before]
        # fill columns 71 through 80
        foreach _ {_ _ _ _ _  _ _ _ _ _} {
            append s [format %c [expr {97+($l%26)}]] ; incr l
        }
        # maybe set the flag again
        if {$f_setat ne ""} {
            append s [format {%c[?7%s} 27 $f_setat]
        }
        # draw three more characters
        foreach _ {_ _ _} {
            append s [format %c [expr {97+($l%26)}]] ; incr l
        }
        # maybe set the flag again
        if {$f_setafter ne ""} {
            append s [format {%c[?7%s} 27 $f_setafter]
        }
        # draw three more characters
        foreach _ {_ _ _} {
            append s [format %c [expr {97+($l%26)}]] ; incr l
        }
        # that's all for that line
        append s "\r\n"
    }

    return $s
}

set patterns(irm1) "Insert/replace mode test"
# "irm1": Tests the insert mode flag, by using it and pseudorandom
# cursor positions to draw the apparent same row, over and over again,
# all down the screen.  The row contents will be:
#       upper case letters, white on red
#       lower case letters, blue on white
#       numerals, yellow on black
#       'a' repeated 18x with different attributes:
#           7x foreground colors, on black
#           black, on 7x background colors
#           plain
#           underline
#           bold
#           underline & bold
# Test by comparing the rows with each other and this description.
# Since it's pseudorandom you can use "srand *" to get different
# variants of this test.  Can also compare against other terminals.
# May also be interesting to run it with the screen full of junk.
#
# It's slow: 26 seconds at 115.2 kbaud.
proc pat_irm1 {} {
    global height
    set width 80

    # Info about the final target apperance: strings describing each char.
    set targ [list]
    lappend targ "*" ; # column zero, doesn't normally show up
    for {set i 0} {$i < 26} {incr i} {
        lappend targ [format {%c[;37;41m%c} 27 [expr {65+$i}]]
    }
    for {set i 0} {$i < 26} {incr i} {
        lappend targ [format {%c[;31;47m%c} 27 [expr {97+$i}]]
    }
    for {set i 0} {$i < 10} {incr i} {
        lappend targ [format {%c[;33;40m%c} 27 [expr {48+$i}]]
    }
    for {set i 0} {$i < 7} {incr i} {
        lappend targ [format {%c[;%u;40ma} 27 [expr {31+$i}]]
    }
    for {set i 0} {$i < 7} {incr i} {
        lappend targ [format {%c[;%u;30ma} 27 [expr {41+$i}]]
    }
    for {set i 0} {$i < 4} {incr i} {
        lappend targ [format {%c[%s%sma} \
            27 \
            [expr {($i & 1) ? ";4" : ""}] \
            [expr {($i & 2) ? ";1" : ""}]]
    }

    # Build a list of pseudorandom cursor moves and mode switches.
    # Can't decide yet what character to insert, because they move around,
    # but keeps track of which character (by insertion index) ends up
    # where, so that later we can pick what character to insert when.
    # Info in:
    #   $actions - list of things from the following
    #       irm 0|1 - insert mode on/off
    #       move $row $col - cursor move
    #       char - emit a character with its attributes, TBD
    #   $screen($row $col) - info about what's at this position:
    #       "" - not yet determined
    #       num - index into $actions, where character was written
    #   $screen() - number of "" in the array
    #   $rscreen($num) - reverse mapping
    #   $irm - current insert mode setting 0 or 1 (or "" if indeterminate)
    #   $row, $col - current position (or "" if indeterminate)
    set actions [list]
    set irm ""
    set screen() 0
    for {set row 1} {$row <= $height} {incr row} {
        for {set col 1} {$col <= $width} {incr col} {
            set screen([list $row $col]) ""
            incr screen()
        }
    }
    set row ""
    set col ""
    while {$screen() > 0} {
        if {$irm eq "" || rand() < 0.02} {
            # set mode on/off
            set irm [expr {rand() < 0.6}]
            lappend actions [list irm $irm]
        } elseif {$row eq "" || $col eq "" || rand() < 0.2} {
            # set cursor position
            set row [expr {int(rand()*$height+1)}]
            switch -- [expr {int(rand()*10)}] {
                0 { set col 1 }
                1 { set col $width }
                default { set col [expr {int(rand()*$width+1)}] }
            }
            lappend actions [list move $row $col]
        } else {
            # emit character; account for it in $screen(); and cursor moves
            if {$irm} {
                # Insert mode
                if {$screen([list $row $width]) eq ""} {
                    incr screen() -1
                }
                for {set col2 $width} {$col2 > $col} {incr col2 -1} {
                    set screen([list $row $col2]) \
                        $screen([list $row [expr {$col2 - 1}]])
                }
            } else {
                # Replace mode
                if {$screen([list $row $col]) eq ""} {
                    incr screen() -1
                }
            }
            set screen([list $row $col]) [llength $actions]
            lappend actions [list char]
            if {$col >= $width} {
                set col ""
                set row ""
            } else {
                incr col
            }
        }
    }
    for {set row 1} {$row <= $height} {incr row} {
        for {set col 1} {$col <= $width} {incr col} {
            set rscreen($screen([list $row $col])) [list $row $col]
        }
    }

    # Internal sanity checks.
    for {set row 1} {$row <= $height} {incr row} {
        for {set col 1} {$col <= $width} {incr col} {
            set k [list $row $col]
            set x $screen($k)
            if {$screen($k) eq ""} {
                error "internal error: empty string still present @ $k"
            } elseif {![info exists rscreen($x)]} {
                error "internal error: rscreen missing entry for $x @ $k"
            } elseif {$rscreen($x) ne $k} {
                error "internal error: rscreen duplicate, $k / $rscreen($x)"
            }
        }
    }

    # Now actually build up the resulting string, full of a lot of escape
    # codes.
    set s ""
    set num 0
    foreach a $actions {
        switch -- [lindex $a 0] {
            irm {
                append s [format {%c[4%s} \
                    27 [expr {[lindex $a 1] ? "h" : "l"}]]
            }
            move {
                append s [format {%c[%d;%dH} \
                    27 [lindex $a 1] [lindex $a 2]]
            }
            char {
                if {[info exists rscreen($num)]} {
                    append s [lindex $targ [lindex $rscreen($num) 1]]
                } else {
                    # this should get erased eventually, so just pick
                    # something random
                    append s [lindex $targ [expr {int(rand()*[llength $targ])}]]
                }
            }
        }
        incr num
    }

    append s [format {%c[4l%c[m} 27 27]

    return $s
}

set patterns(irm2) "Insert/replace mode test II"
# "irm2": Tests the insert mode flag, by inserting single characters in
# staggered positions over a patterened grid.  Test by comparing to another
# terminal.
proc pat_irm2 {} {
    global height
    set width 80
    set s ""

    # build the grid
    for {set row -1} {$row <= $height} {incr row} {
        append s "\r\n"
        # letter 'a' with various attributes
        for {set col 1} {$col <= $width} {incr col} {
            set atts [list]
            lappend atts "" ; # clear attributes
            lappend atts [expr {30 + ($col & 7)}] ; # fg color
            lappend atts [expr {47 - ($col & 7)}] ; # bg color
            if {$col & 8} {
                lappend atts 7 ; # inverse
            }
            if {$col & 16} {
                lappend atts 4 ; # underline
            }
            append s [format {%c[%sma} 27 [join $atts ";"]]
        }
    }

    # do insertions
    append s [format {%c[4h%c[m} 27 27]
    for {set row 1} {$row <= $height} {incr row} {
        set cols [list]
        set col $row
        while {$col <= $width} {
            lappend cols $col
            incr col [expr {max(7,($height>>1)-1)}]
        }
        foreach col $cols {
            append s [format {%c[%d;%dH%c[m*} 27 $row $col 27]
        }
    }
    append s [format {%c[4l%c[m} 27 27]

    return $s
}

## ## ## ## ## end of individual test patterns

if {[info exists patterns($pattern)]} {
    set patdat [pat_$pattern]
} else {
    puts stderr "Unrecognized pattern name '$pattern'"
    puts stderr "Available:"
    foreach pattern [lsort [array names patterns]] {
        puts stderr [format "%10s - %s" $pattern $patterns($pattern)]
    }
    exit 1
}

# Convert the CSI if desired.
if {$highcsi > 0} {
    set patdat2 ""
    set len [string length $patdat]
    for {set i 0} {$i < $len} {incr i} {
        if {[string index $patdat $i] eq "\033" &&
            [string index $patdat ${i}+1] eq "\[" &&
            ($highcsi >= 1.0 || $highcsi > rand())} {
            append patdat2 "\233"
            incr i
        } else {
            append patdat2 [string index $patdat $i]
        }
    }
    set patdat $patdat2
}

# Reset if desired.
if {$reset} {
    puts -nonewline $fp [format %cc 27]
    flush $fp
    after $resetms
}

# Now that we have a pattern, in $patdat, transmit it to the device.
set patlen [string length $patdat]
for {set patidx 0} {$patidx < $patlen} {incr patidx} {
    set patbyt [string index $patdat $patidx]
    if {$patbyt eq "\033" &&
        [string range $patdat 1+$patidx 3+$patidx] eq "\[=="} {
        # Special commands interpreted here & not passed on to the device.
        incr patidx 4
        set num ""
        while {1} {
            set patbyt [string index $patdat $patidx]
            if {$patbyt ne "" && [string is digit -strict $patbyt]} {
                append num $patbyt
            } else {
                break
            }
            incr patidx
        }
        if {$patbyt eq "|"} {
            # programmed delay
            after $num
        } else {
            # unknown!
            error "Unknown private sequence enter '$patbyt'"
        }
    } else {
        puts -nonewline $fp $patbyt
        flush $fp
        if {$slowdown} { after $slowdown }
    }
}

# if desired, sleep
if {$linger} {
    after 300000
}

exit 0
