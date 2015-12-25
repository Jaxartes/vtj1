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

# Test the LEDs for vtj1.asm.  The general syntax is:
#   sudo tclsh tst19leds.tcl [options]
# This requires constant user intervention, to look at the LEDs and see
# what they look like.

set dev /dev/ttyUSB1
set baud 115200
set reset 0
set boardleds 5
expr {srand(14703)}
foreach mod $argv {
    if {$mod eq "reset"} {
        set reset 1
    } elseif {[string match "srand *" $mod]} {
        expr {srand([lindex $mod 1])}
    } elseif {[string match "baud *" $mod]} {
        # set baud rate other than 115200
        set baud [string range $mod 5 end]
    } elseif {[string match "boardleds *" $mod]} {
        # set number of LEDs on the circuit board as opposed to the keyboard
        set boardleds [string range $mod 10 end]
    } else {
        puts stderr "Unknown keyword $mod, ignored."
    }
}

puts stderr "dev = $dev"
puts stderr "baud = $baud"
puts stderr "boardleds = $boardleds"

set fp [open $dev r+]
fconfigure $fp -mode ${baud},n,8,1
fconfigure $fp -buffering none -encoding binary -translation binary -blocking 1
if {$dev ne ""} {
    fconfigure $fp -handshake xonxoff
}

if {$reset} {
    puts -nonewline $fp [format %cc 27]
    flush $fp
    after 15000
}

# start with all LEDs off & all modifiers off
for {set i 1} {$i <= (3 + $boardleds)} {incr i} {
    set ledst($i) 0
}
for {set i 1} {$i <= 3} {incr i} {
    set modst($i) 0
}

# now perform operations; randomly selecting operations & keeping track
# of state
for {set op 0} {1} {incr op} {
    puts stderr "op=$op"
    if {rand() < 0.6} {
        # Let the user know what the state should be, so they can examine it
        set msg ""
        if {$ledst(2)} { append msg " +num" }
        if {$ledst(3)} { append msg " +caps" }
        if {$ledst(1)} { append msg " +scroll" }
        for {set i 1} {$i <= $boardleds} {incr i} {
            if {$ledst([expr {$i+3}])} { append msg " +LED$i" }
        }
        if {$msg eq ""} { append msg " (all off)" }
        puts stderr "Current state: $msg"
        puts -nonewline stderr "Press enter here after checking."
        gets stdin
    }
    if {rand() < 0.3} {
        # Have the user press a key
        set i [expr {int(rand()*3+1)}]
        switch -- $i {
            1 { set k "ScrLk" }
            2 { set k "Num Lock" }
            3 { set k "Caps Lock" }
            default { error "Internal error with key numbering" }
        }
        puts -nonewline stderr "Press the \"$k\" key, then press enter here."
        gets stdin
        set modst($i) [expr {!$modst($i)}]
        set ledst($i) $modst($i)
    } else {
        # Issue an escape sequence to maybe change the LEDs.
        set parms [list]
        while {[llength $parms] < int(rand()*3.5)} {
            if {rand() < 0.2} {
                if {rand() < 0.5} {
                    lappend parms ""
                } else {
                    lappend parms 0
                }
            } elseif {rand() < 0.8} {
                lappend parms [expr {int(rand()*($boardleds+3)+1)}]
            } elseif {rand() < 0.5} {
                lappend parms [expr {int(rand()*65536)}]
            } else {
                lappend parms [expr {int(rand()*256)}]
            }
        }
        puts -nonewline $fp [format {%c[%sq} 27 [join $parms ";"]]
        puts stderr [format {Transmitted: ^[[%sq} [join $parms ";"]]
        set p0 [lindex $parms 0]
        if {$p0 eq ""} {
            set p0 0
        }
        if {$p0 == 0} {
            # turn off all LEDs
            for {set i 1} {$i <= (3 + $boardleds)} {incr i} {
                set ledst($i) 0
            }
        } elseif {$p0 <= (3 + $boardleds)} {
            # turn on one LED
            set ledst($p0) 1
        }
    }
}
