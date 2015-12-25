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

# test cases for four cursor movement escape sequences in VTJ-1, namely
# ^[[A, ^[[B, ^[[C, ^[[D.  Draws something on the screen that can be
# compared between VTJ-1 and perhaps xterm.

# openings

set dev /dev/ttyUSB1
set baud 115200
set pattern [lindex $argv 0]
set textsz 40
set linger 0
set reset 0
set width 80
foreach mod [lrange $argv 1 end] {
    if {$mod eq "stdout"} {
        set dev ""
    } elseif {$mod eq "linger"} {
        set linger 1
    } elseif {$mod eq "reset"} {
        set reset 1
    } elseif {$mod eq "width78"} {
        set width 78
    } else {
        puts stderr "Unknown keyword $mod, ignored."
    }
}

puts stderr "dev = $dev"
puts stderr "baud = $baud"
puts stderr "pattern = $pattern"
puts stderr "textsz = $textsz"

if {$dev eq ""} {
    set fp stdout
    set baud 115200
} else {
    set fp [open $dev r+]
    fconfigure $fp -buffering none -encoding binary -translation binary \
        -blocking 0 -mode ${baud},n,8,1
}

# Predictable pseudo random number generation. (Using MINSTD.)
set prngs [expr {147036 + 258147 * $pattern}]
proc prng {} {
    global prngs
    set prngs [expr {($prngs * 48271) % 0x7fffffff}]
    return $prngs
}

# Decode $pattern which is supposed to identify the pattern to run.
# It's a number 0-9.  The only difference numbers beyond that range make
# is to seed the pseudorandom number generator differently.
# It derives three values
#       cuorg - origin mode on (1) or off (0)
#       cusrg - scroll region type (0 - 3: full, top, bottom, middle)
#       custa - starting point above (1) or within (0) scroll region
switch -- [expr {$pattern % 10}] {
    0 { set cuorg 0 ; set cusrg 0 ; set custa 0 }
    1 { set cuorg 0 ; set cusrg 1 ; set custa 0 }
    2 { set cuorg 0 ; set cusrg 2 ; set custa 0 }
    3 { set cuorg 0 ; set cusrg 2 ; set custa 1 }
    4 { set cuorg 0 ; set cusrg 3 ; set custa 0 }
    5 { set cuorg 0 ; set cusrg 3 ; set custa 1 }
    6 { set cuorg 1 ; set cusrg 0 ; set custa 0 }
    7 { set cuorg 1 ; set cusrg 1 ; set custa 0 }
    8 { set cuorg 1 ; set cusrg 2 ; set custa 0 }
    9 { set cuorg 1 ; set cusrg 3 ; set custa 0 }
    default { error "bug processing pattern number" }
}

puts stderr [list pattern $pattern cuorg $cuorg cusrg $cusrg custa $custa]

# And from that derive some more:
#       cutop - top Y coordinate of scroll region
#       cubot - bottom Y coordinate of scroll region
#       cusrs - code to set scroll region
#       custx - starting point X coordinate
#       custy - starting point Y coordinate
set cutop [expr {($cusrg & 2) ? [expr {2 + ([prng] & 7)}] : 1}]
set cubot [expr {$textsz - (($cusrg & 1) ? [expr {1 + ([prng] & 7)}] : 0)}]
set cusrs [format {%c[%d;%dr} 27 $cutop $cubot]
set custx [expr {1 + ([prng] % $width)}]
if {$custa} {
    set custy [expr {1 + ([prng] % ($cutop - 1))}]
} else {
    set custy [expr {$cutop + ([prng] % ($cubot - $cutop + 1))}]
}

puts stderr \
    [list cutop $cutop cubot $cubot custx $custx custy $custy]

# Gradual transmission of data.
proc emit {s} {
    global fp baud
    foreach b [split $s ""] {
        puts -nonewline $fp $b
        flush $fp
        after [expr {int(ceil(25000 / $baud))}]
    }
}

# Reset if desired.
if {$reset} {
    emit [format %cc 27]
    after 15000
}

# Home position, full-screen scroll, and clear the screen
emit [format {%c[m%c[r%c[2J} 27 27 27]

# draw screen border
set bofg [expr {[prng] % 8}]
set bobg [expr {(1 + ([prng] % 7)) ^ $bofg}]
set boch [format %c [expr {([prng] % 26) + 32 * ([prng] % 2) + 65}]]
emit [format {%c[3%d;4%dm} 27 $bofg $bobg]
emit [string repeat $boch $width]
for {set y 2} {$y < $textsz} {incr y} {
    emit "\r\n"
    emit $boch
    emit [format {%c[%dC} 27 [expr {$width - 2}]]
    emit $boch
}
emit "\r\n"
emit [string repeat $boch $width]

# now set origin mode, and back to default coloring
emit [format {%c[m%c[?6%s} 27 27 [expr {$cuorg ? "h" : "l"}]]

# now draw the lines
foreach dc {A B C D} {
    # set scroll region & go to home position
    emit $cusrs
    set x 1
    set y [expr {$cuorg ? $cutop : 1}]
    # move to start position
    emit [format {%c[%dB%c[%dC} 27 [expr {$custy - $y}] 27 [expr {$custx - $x}]]
    # draw the line
    for {set i 0} {$i < 52} {incr i} {
        emit [format %c%c [expr {65 + ($i % 26) + 32*int($i/26)}] 8]
        emit [format {%c[%s} 27 $dc]
    }
}

# done, now clear up our state somewhat

emit [format {%c[m} 27]
if {$linger} {
    after 300000
}

exit 0
