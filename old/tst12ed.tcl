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

# test cases for "ED" (half or full screen erase) sequence in VTJ-1.

# openings

set dev /dev/ttyUSB1
set baud 115200
set pattern [lindex $argv 0]
set textsz 40
set home [format {%c[r} 27]
set linger 0
set reset 0
set width 80
foreach mod [lrange $argv 1 end] {
    if {$mod eq "stdout"} {
        set dev ""
    } elseif {$mod eq "realhome"} {
        set home [format {%c[H} 27]
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

# Decode $pattern which is supposed to identify the pattern to run.
# It's a number 0-107:
#       mod 3 - choice of ED 0, 1, 2
#       div 3 mod 3 - at left, middle, right of row
#       div 9 mod 3 - at top, middle, bottom of screen
#       div 27 mod 4 - on row with the various row attributes
set edop [expr {$pattern % 3}]
switch -- [expr {int($pattern / 3) % 3}] {
    0 { set edx 0 }
    1 { set edx 79 }
    2 { set edx 33 }
}
switch -- [expr {int($pattern / 9) % 3}] {
    0 { set edy 0 }
    1 { set edy [expr {int($textsz * 0.6)}] }
    2 { set edy [expr {$textsz - 1}] }
}
set edra [expr {int($pattern / 27) % 4}]
puts stderr [list $pattern edop $edop edx $edx edy $edy edra $edra]

# Gradual transmission of data.
proc emit {s} {
    global fp baud
    foreach b [split $s ""] {
        puts -nonewline $fp $b
        flush $fp
        after [expr {int(ceil(25000 / $baud))}]
    }
}

# Predictable pseudo random number generation. (Using MINSTD.)
set prngs [expr {147036 + 258147 * $pattern}]
proc prng {} {
    global prngs
    set prngs [expr {($prngs * 48271) % 0x7fffffff}]
    return $prngs
}

# Reset if desired.
if {$reset} {
    emit [format %cc 27]
    after 15000
}

# Home position and clear the screen
emit [format {%c[m%s%c[2J} 27 $home 27]

# Pick colors
set colorlist [list]
for {set i 0} {$i < 7} {incr i} {
    lappend colorlist [expr {(([prng] & 0xffff) << 3) | $i}]
}
set colorlist2 [list]
foreach c [lsort -int $colorlist] {
    lappend colorlist2 [expr {$c & 7}]
}
set nfg [lindex $colorlist2 0]
set nbg [lindex $colorlist2 1]
set ofgs [list]
set obgs [list]
foreach {f b} [lrange $colorlist2 2 end] {
    lappend ofgs $f
    lappend obgs $b
}

# Fill the screen with text, one row at a time.
set erste 1
for {set y 0} {$y < $textsz} {incr y} {
    # new line, unless this is the first
    if {$y} {
        emit "\r\n"
    }
    # row attributes
    set r [expr {[prng] & 0xffff}]
    if {$y == $edy} {
        set ra $edra
    } elseif {$r % 3} {
        set ra 0
    } else {
        set ra [expr {$r & 3}]
    }
    switch -- $ra {
        0 { }
        1 { emit [format {%c#6} 27] }
        2 { emit [format {%c#3} 27] }
        3 { emit [format {%c#4} 27] }
    }
    set ewidth $width
    if {$ewidth > 40 && $ra} {
        # I don't like what 'xterm' does past column 40 of double width rows.
        set ewidth 40
    }
    # characters
    for {set x 0} {$x < $ewidth} {incr x} {
        if {$erste || !([prng] % 15)} {
            # change text attributes including colors
            set ops [list]
            set r [expr {[prng] & 0xffff}]
            if {$erste || ($r % 3) < 1} {
                # FG color
                lappend ops 3[lindex $ofgs [expr {[prng]%[llength $ofgs]}]]
            }
            if {$erste || ($r % 5) < 2} {
                # BG color
                lappend ops 4[lindex $obgs [expr {[prng]%[llength $obgs]}]]
            }
            if {($r % 11) < 1} {
                # bold
                lappend ops 1
            }
            if {($r % 13) < 1} {
                # underline
                lappend ops 4
            }
            if {($r % 23) < 2} {
                # inverse video
                lappend ops 7
            }
            if {($r % 29) < 4} {
                # no bold
                lappend ops 22
            }
            if {($r % 31) < 4} {
                # no underline
                lappend ops 24
            }
            if {($r % 37) < 6} {
                # no inverse
                lappend ops 27
            }
            set erste 0
            emit [format {%c[%sm} 27 [join $ops {;}]]
        }
        if {[prng] % 5} {
            emit [format {%c} [expr {([prng]%26)+97}]]
        } else {
            emit " "
        }
    }
}

# Now position the cursor to where we want to achieve ED.  You may have
# noticed that I'm not using cursor positioning commands, just ^[[r.
# That's really for setting the scroll region but it has the side effect
# of putting the cursor on the upper left corner of the screen.

# And I haven't implemented the cursor positioning commands yet.

# Back to top of screen
emit $home

# And down to designated row
emit [string repeat "\r\n" $edy]

# And to the designated column: tab to or past it, then backspace
set x 0
while {$x < 79 && $x < $edx} {
    emit "\t"
    incr x 8
    if {$x > 79} { set x 79 }
}
while {$x > $edx} {
    emit "\b"
    incr x -1
}

# Now perform the clear
emit [format {%c[%dJ} 27 $edop]

# And fill in every line with a dummy marker just to detect row attributes
emit [format {%c[;3%d;4%dm%s} 27 $nfg $nbg $home]
for {set y 0} {$y < $textsz} {incr y} {
    if {$y} {
        emit "\r\n"
    }
    if {!$edx} {
        emit "\t"
    }
    emit "\$"
}

emit [format {%c[m} 27]
if {$linger} {
    after 300000
}

exit 0
