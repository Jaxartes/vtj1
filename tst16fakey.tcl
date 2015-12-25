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

# Test vtj1.asm by "faking" key codes, to see what it sends back

set dev /dev/ttyUSB1
set baud 115200
set reset 0
set moremods 1
set cursorkeymode 0
set lfnlmode 0
set lockmode 0
expr {srand(14703)}
foreach mod $argv {
    if {$mod eq "reset"} {
        set reset 1
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
    } elseif {$mod eq "lessmods" } {
        set moremods 0
    } elseif {$mod eq "cursorkeymode" } {
        set cursorkeymode 1
    } elseif {$mod eq "lockmode" } {
        set lockmode 1
    } elseif {[string match "dev *" $mod]} {
        set dev [lindex $mod 1]
    } elseif {[string match "baud *" $mod]} {
        # set baud rate other than 115200
        set baud [string range $mod 5 end]
    } elseif {$mod eq "lfnlmode" } {
        set lfnlmode 1
    } else {
        puts stderr "Unknown keyword $mod, ignored."
    }
}

puts stderr "dev = $dev"
puts stderr "baud = $baud"

    set fp [open $dev r+]
    fconfigure $fp -buffering none -encoding binary -translation binary \
        -blocking 1 -mode ${baud},n,8,1

# Reset if desired.
if {$reset} {
    puts -nonewline $fp [format %cc 27]
    flush $fp
    after 15000
}

# Eliminate any already buffered input.
fconfigure $fp -blocking 0
while {1} {
    update
    set got [read $fp 512]
    if {$got eq ""} { break }
    after 100
}
fconfigure $fp -blocking 1

# Table of so-far recognized keycodes; for each:
#       label; prefix with NO to suppress the key
#       hex of numeric keycodes for make
#       hex of numeric keycodes for break
#       string sent back if any, unmodified
#       string sent back if any, with caps lock
#           if this isn't blank, caps lock and shift cancel each other out
#           so for non alphabetic characters this should be blank ("")
#       string sent back if any, controlled
#       string sent back if any, shifted
#       empty string, making room for "alt" later or maybe "num"
#       modifier effect or other special key function:
#           "s"hift modifier
#           "c"ontrol modifier
#           "k" caps lock modifier (sticky)
#           "-" nothing
#           "a" this is an arrow key; generates escape sequence and ignores
#               modifiers; the "unmodified string" field only contains the
#               different final character for each
# kdata1 - raw ones, with all the above
# kdata2 - alphabetical ones, with just:
#       lower case letter
#       hex digits of make code
set kdata1 {
    "caps"    58   f058   "" "" "" "" "" k
    "l shft"  12   f012   "" "" "" "" "" s
    "l ctrl"  14   f014   "" "" "" "" "" c
    "NOl gui" e01f e0f01f "" "" "" "" "" -
    "NOl alt" 11   f011   "" "" "" "" "" -
    "r shft"  59   f059   "" "" "" "" "" s
    "r ctrl"  e014 e0f014 "" "" "" "" "" c
    "NOr gui" e027 e0f027 "" "" "" "" "" -
    "NOr alt" e011 e0f011 "" "" "" "" "" -

    "NOapps"  e02f e0f02f "" "" "" "" "" -

    "0" 45 f045 "0" "" "0"    ")"  "" -
    "1" 16 f016 "1" "" "1"    "!"  "" -
    "2" 1e f01e "2" "" "\000" "@"  "" -
    "3" 26 f026 "3" "" "\033" "\#" "" -
    "4" 25 f025 "4" "" "\034" "\$" "" -
    "5" 2e f02e "5" "" "\035" "%"  "" -
    "6" 36 f036 "6" "" "\036" "^"  "" -
    "7" 3d f03d "7" "" "\037" "&"  "" -
    "8" 3e f03e "8" "" "\177" "*"  "" -
    "9" 46 f046 "9" "" "9"    "("  "" -

    "bkqu" 0e f00e "`"  "" "\036" "~" "" -
    "hyph" 4e f04e "-"  "" "-"    "_" "" -
    "equa" 55 f055 "="  "" "="    "+" "" -
    "bsla" 5d f05d "\\" "" "\034" "|" "" -
    "lsqb" 54 f054 "\[" "" "\033" "{" "" -
    "rsqb" 5b f05b "\]" "" "\035" "}" "" -
    "semi" 4c f04c "\;" "" "\;"   ":" "" -
    "squo" 52 f052 "\'" "" "\'"   "\"" "" -
    "coma" 41 f041 ","  "" ","    "<" "" -
    "dot"  49 f049 "."  "" "."    ">" "" -
    "fsla" 4a f04a "/"  "" "\037" "?" "" -

    "bksp"  66 f066 "\010" "" "\010" "\010" "" -
    "space" 29 f029 "\040" "" "\000" "\040" "" -
    "tab"   0d f00d "\011" "" "\011" "\011" "" -
    "enter" 5a f05a "\015" "" "\015" "\015" "" -
    "esc"   76 f076 "\033" "" "\033" "\033" "" -

    "NOf1"  05 f005 "" "" "" "" "" -
    "NOf2"  06 f006 "" "" "" "" "" -
    "NOf3"  04 f004 "" "" "" "" "" -
    "NOf4"  0c f00c "" "" "" "" "" -
    "NOf5"  03 f003 "" "" "" "" "" -
    "NOf6"  0b f00b "" "" "" "" "" -
    "NOf7"  83 f083 "" "" "" "" "" -
    "NOf8"  0a f00a "" "" "" "" "" -
    "NOf9"  01 f001 "" "" "" "" "" -
    "NOf10" 09 f009 "" "" "" "" "" -
    "NOf11" 78 f078 "" "" "" "" "" -
    "NOf12" 07 f007 "" "" "" "" "" -

    "NOprnt_scrn" e012e07c e0f07ce0f012 "" "" "" "" "" -
    "NOscroll" 7e f07e "" "" "" "" "" -
    "NOpause" e11477e1f014f077 {} "" "" "" "" "" -

    "NOins"  e070 e0f070 "" "" "" "" "" -
    "NOhome" e06c e0f06c "" "" "" "" "" -
    "NOpgup" e07d e0f07d "" "" "" "" "" -
    "NOdele" e071 e0f071 "" "" "" "" "" -
    "NOend"  e069 e0f069 "" "" "" "" "" -
    "NOpgdn" e07a e0f07a "" "" "" "" "" -
    "upar" e075 e0f075 "A" "" "" "" "" a
    "dnar" e072 e0f072 "B" "" "" "" "" a
    "lfta" e06b e0f06b "D" "" "" "" "" a
    "rgta" e074 e0f074 "C" "" "" "" "" a

    "NOnum" 77 f077 "" "" "" "" "" -

    "NOkp/" e04a e0f04a "" "" "" "" "" -
    "NOkp*" 7c   f07c   "" "" "" "" "" -
    "NOkp-" 7b   f07b   "" "" "" "" "" -
    "NOkp+" 79   f079   "" "" "" "" "" -
    "NOkpe" e05a e0f05a "" "" "" "" "" -
    "NOkp." 71   f071   "" "" "" "" "" -
    "NOkp0" 70   f070   "" "" "" "" "" -
    "NOkp1" 69   f069   "" "" "" "" "" -
    "NOkp2" 72   f072   "" "" "" "" "" -
    "NOkp3" 7a   f07a   "" "" "" "" "" -
    "NOkp4" 6b   f06b   "" "" "" "" "" -
    "NOkp5" 73   f073   "" "" "" "" "" -
    "NOkp6" 74   f074   "" "" "" "" "" -
    "NOkp7" 6c   f06c   "" "" "" "" "" -
    "NOkp8" 75   f075   "" "" "" "" "" -
    "NOkp9" 7d   f07d   "" "" "" "" "" -
}
set kdata2 {
    a 1c b 32 c 21 d 23 e 24 f 2b g 34 h 33 i 43 j 3b
    k 42 l 4b m 3a n 31 o 44 p 4d q 15 r 2d s 1b t 2c
    u 3c v 2a w 1d x 22 y 35 z 1a
}

# put those into arrays
#   $kdb($lbl,$fld) = field value
#   $kdb() = list of labels
#   $kdb(M) = list of labels for modifier keys only
set kdb() [list]
set kdb(M) [list]
foreach {lbl mkx brx str sca scl ssh sxx mod} $kdata1 {
    foreach fld {lbl mkx brx str sca scl ssh sxx mod} {
        upvar 0 $fld val
        set kdb(${lbl},${fld}) $val
    }
    lappend kdb() $lbl
    if {$mod ne "-" && $mod ne "a"} {
        lappend kdb(M) $lbl
    }
}
foreach {low mk} $kdata2 {
    set kdb(${low},lbl) $low
    set kdb(${low},mkx) $mk
    set kdb(${low},brx) "f0$mk"
    set kdb(${low},str) $low
    set kdb(${low},sca) [string toupper $low]
    set kdb(${low},scl) [format %c [expr {[scan $low %c] - 96}]]
    set kdb(${low},ssh) [string toupper $low]
    set kdb(${low},sxx) ""
    set kdb(${low},mod) -
    lappend kdb() $low
}

# sanitize - make a string printable
proc sanitize {s} {
    set s2 ""
    for {set i 0} {$i < [string length $s]} {incr i} {
        set c [string index $s $i]
        if {$c ne "\\" && $c ne "\"" && ![string is print -strict $c]} {
            append s2 [format "\\%03o" [scan $c %c]]
        } else {
            append s2 $c
        }
    }
    return $s2
}

if {1} {
    # For debugging purposes, dump the kdb() array contents
    puts stderr "kdb(...) = "
    foreach lbl $kdb() {
        puts stderr "\tLabel \"[sanitize $lbl]\""
        foreach fld {lbl mkx brx str sca scl ssh sxx mod} {
            set val [sanitize $kdb(${lbl},${fld})]
            puts stderr "\t\t Field $fld = \"$val\""
        }
    }
}

# enable or disable cursor key mode
puts -nonewline $fp [format {%c[?1%s} 27 [expr {$cursorkeymode ? "h" : "l"}]]

# and line feed / newline mode
puts -nonewline $fp [format {%c[20%s} 27 [expr {$lfnlmode ? "h" : "l"}]]

# fakey() - format a "fake key" escape sequence based on hex characters
proc fakey {hex} {
    set bytes [list]
    for {set i 0} {1} {incr i 2} {
        set hexb [string range $hex $i 1+$i]
        if {$hexb eq ""} break
        if {[string length $hexb] ne 2} {
            error "Bad hex string $hex"
        }
        lappend bytes [scan $hexb %02x]
    }
    set s [format {%c[184;%s|} 27 [join $bytes ";"]]
    return $s
}

# Now, at intervals, transmit the code for a faked key event, picked
# pseudorandomly, and observe the results.
set ectr 0
set made [list] ; # keys down, by label
array set mods [list k 0 s 0 c 0 - 0 a 0]
array set modsticky [list k 1 s 0 c 0 - 0 a 0]
set lockmode_active 0
while {1} {
    after 40
    incr ectr
    set nmade [llength $made]
    if {$lockmode && rand() < ($lockmode_active ? 0.3 : 0.1)} {
        # Enable or disable keyboard lock mode
        set lockmode_active [expr {!$lockmode_active}]
        puts -nonewline $fp \
            [format {%c[2%s} 27 [expr {$lockmode_active ? "h" : "l"}]]
    }
    set i [expr {int(rand()*(1+$nmade))}]
    if {$i >= $nmade} {
        # A "make" event for a key that's not down yet.

        # Pick a key
        while {1} {
            if {$moremods && rand() < 0.3} {
                set lbl [lindex $kdb(M) [expr {int(rand()*[llength $kdb(M)])}]]
            } else {
                set lbl [lindex $kdb() [expr {int(rand()*[llength $kdb()])}]]
            }
            if {[string match "NO*" $lbl]} {
                # this one is not currently programmed
                continue
            }
            if {[lsearch -exact $made $lbl] >= 0} {
                # this one is already down
                continue
            }
            break
        }

        # And what happens as a result.
        set es [fakey $kdb(${lbl},mkx)]
        set ed "make($lbl)"

        # Account for the state change it makes
        lappend made $lbl
        set m $kdb(${lbl},mod)
        if {$modsticky($m)} {
            set mods($m) [expr {!$mods($m)}]
        } else {
            set mods($m) 1
        }
        ### puts stderr "mods: [array get mods]"

        # And what results do we expect from that?
        if {$kdb(${lbl},mod) eq "a"} {
            # arrow key
            set er [format {%c%c%s} \
                27 \
                [expr {$cursorkeymode ? 79 : 91}] \
                $kdb(${lbl},str)]
        } elseif {$mods(c)} {
            # modified by "control"
            set er $kdb(${lbl},scl)
        } elseif {$kdb(${lbl},sca) ne ""} {
            # caps lock & shift will cancel each other out on this key
            if {$mods(k) && $mods(s)} {
                # and they do
                set er $kdb(${lbl},str)
            } elseif {$mods(k)} {
                # but not this time (caps lock)
                set er $kdb(${lbl},sca) 
            } elseif {$mods(s)} {
                # nor this time (shift)
                set er $kdb(${lbl},ssh)
            } else {
                # this time they're not even down
                set er $kdb(${lbl},str)
            }
        } else {
            # caps lock will be ignored on this key
            if {$mods(s)} {
                # shifted
                set er $kdb(${lbl},ssh)
            } else {
                # unshifted
                set er $kdb(${lbl},str)
            }
        }
        if {$lfnlmode && $lbl eq "enter"} {
            # In linefeed / newline mode, the enter/return key generates
            # both CR and LF characters.  Add the LF now.
            append er "\012"
        }
    } else {
        # A "break" event for a key that was down.

        # Figure out what happens.
        set lbl [lindex $made $i]
        set es [fakey $kdb(${lbl},brx)]
        set ed "break($lbl)"

        # Account for the state change it makes.
        set made [concat [lrange $made 0 ${i}-1] [lrange $made ${i}+1 end]]
        set m $kdb(${lbl},mod)
        if {!$modsticky($m)} {
            set mods($m) 0
        }

        # And we don't expect any results from that.
        set er ""
    }

    if {$lockmode_active} {
        # In lock mode it shouldn't transmit characters that come from the
        # keyboard.
        set er ""
    }

    # Transmit our commands to the VTJ-1
    puts -nonewline $fp $es

    # Let the user know about it
    puts stderr "$ectr $ed -- transmitted [sanitize $es]"

    if {$er ne ""} {
        # Response expected.
        puts stderr "$ectr expecting response: [sanitize $er]"
        while {$er ne ""} {
            set eg [read $fp 1]
            if {$eg eq [string index $er 0]} {
                # got the next character we were expecting
                set er [string range $er 1 end]
            } elseif {$eg eq "\021"} {
                # ignore XON
            } elseif {$eg eq "\017"} {
                # the only thing this script does about XOFF is
                # fake it by adding a one second delay
                after 1000
            } else {
                error "Expected char [sanitize $er] got [sanitize $eg]"
            }
        }
    }
}

exit 0
