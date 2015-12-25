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

# vtt_analyze.tcl
# Interact over the serial port with VTJ-1 and use the development escape
# sequence "ESC[23|" to generate reports of the video and other timings,
# and analyze the results.
# Parameters:
#       + expression defining the matching function
#       + threshold
#       + approximate runtime in seconds 

set dev /dev/ttyUSB1
set baud 115200
set targmhz 74.666667
set mexpr [lindex $argv 0]
set thresh [lindex $argv 1]
set runsec [lindex $argv 2]

puts stderr "dev = $dev"
puts stderr "baud = $baud"
puts stderr "targmhz = $targmhz"
puts stderr "mexpr = $mexpr"
puts stderr "thresh = $thresh"
puts stderr "runsec = $runsec"

#####

# Parse the expression, $mexpr, to get the settings for VTT, namely,
# selection of two channels to monitor & logical function to combine
# them.
# Syntax:
#   <expr> ::= <uexpr> (('&'|'|') <uexpr>)*
#   <uexpr> ::= ['~'] <aexpr>
#   <aexpr> ::= 'vsync' | 'hsync' | 'red' | 'green' | 'blue'
#             | '0' | '1'
#             | '(' <expr> ')'
# The parsing functions will work on a string stored in $mps, its length
# in $mpl, and position in $mpp.  $mp1 and $mp2 indicate the primary
# and secondary channel names, or empty string if not yet assigned.  The parsing
# functions return the 4-bit logic function code.

proc meparse_expr {} {
    global mps mpl mpp
    set u1 [meparse_uexpr]
    while {1} {
        if {$mpp >= $mpl} { return $u1 }
        set o [string index $mps $mpp]
        if {$o eq "&"} {
            incr mpp
            set u2 [meparse_uexpr]
            set u1 [expr {$u1 & $u2}]
        } elseif {$o eq "|"} {
            incr mpp
            set u2 [meparse_uexpr]
            set u1 [expr {$u1 | $u2}]
        } else {
            return $u1
        }
    }
}

proc meparse_uexpr {} {
    global mps mpl mpp
    set nega 0
    if {[string index $mps $mpp] eq "~"} {
        incr mpp
        set nega 1
    }
    set a1 [meparse_aexpr]
    if {$nega} {
        set a1 [expr {$a1 ^ 15}]
    }
    return $a1
}

proc meparse_aexpr {} {
    global mps mpl mpp
    set c [string index $mps $mpp]
    if {$c eq "("} {
        incr mpp
        set e [meparse_expr]
        if {[string index $mps $mpp] ne ")"} {
            error "')' expected, not found, position $mpp in '$mps'"
        } else {
            incr mpp
            return $e
        }
    } elseif {$c eq "0"} {
        incr mpp
        return 0
    } elseif {$c eq "1"} {
        incr mpp
        return 15
    } else {
        foreach w {vsync hsync red green blue pe} {
            set wl [string length $w]
            if {[string range $mps $mpp [expr {$mpp + $wl - 1}]] eq $w} {
                # Match!
                global mp1 mp2
                if {$mp1 eq $w} {
                    # Assigned already as primary channel
                    incr mpp $wl
                    return 10
                } elseif {$mp2 eq $w} {
                    # Assigned already as secondary channel
                    incr mpp $wl
                    return 12
                } elseif {$mp1 eq ""} {
                    # Assign this one as primary channel
                    set mp1 $w
                    incr mpp $wl
                    return 10
                } elseif {$mp2 eq ""} {
                    # Assign this one as secondary channel
                    set mp2 $w
                    incr mpp $wl
                    return 12
                } else {
                    error "Can't select 3 channels - $mp1, $mp2, and now $w"
                }
            }
        }
        error "Atom expected, not found, position $mpp in '$mps'"
    }
}

set mps $mexpr
set mpl [string length $mps]
set mpp 0
set mp1 ""
set mp2 ""
set parsed [meparse_expr]
if {$mpp != $mpl} {
    error "Garbage at end of '$mps', parsed $mpp out of $mpl chars"
}

# And now that it's been parsed, convert it to commands we'll send.
set chansel 0
foreach {cn cs} [list $mp1 0 $mp2 4] {
    switch -- $cn {
        vsync { set cv 0 }
        hsync { set cv 1 }
        pe    { set cv 2 }
        red   { set cv 4 }
        green { set cv 5 }
        blue  { set cv 6 }
        default { set cv 0 }
    }
    set chansel [expr {$chansel + ($cv << $cs)}]
}
set mexpr_cmds [format {%c[92;%u;%u|%c[92;%u;%u|} \
    27 0xb700 $chansel 27 0xb701 $parsed]

# And there are also commands to set the threshold.
set thresh_cmds \
    [format {%c[92;%u;%u|%c[92;%u;%u|%c[92;%u;%u|%c[92;%u;%u|} \
        27 0xb710 [expr {$thresh & 255}] \
        27 0xb711 [expr {($thresh >> 8) & 255}] \
        27 0xb712 [expr {($thresh >> 16) & 255}] \
        27 0xb713 [expr {($thresh >> 24) & 255}]]

set cmds "$mexpr_cmds$thresh_cmds"
proc safestring {s} {
    set r ""
    foreach ch [split $s {}] {
        set cc [scan $ch %c]
        if {$cc < 32 || $cc > 126} {
            append r [format {\\%03o} $cc]
        } elseif {$ch eq "\\"} {
            append r "\\\\"
        } else {
            append r $ch
        }
    }
    return $r
}
puts stderr "Will issue the following commands:\n[safestring $cmds]"

#####

# connect

set fp [open $dev r+]
fconfigure $fp -buffering none -encoding binary -translation binary \
    -blocking 0 -mode ${baud},n,8,1

# consume any garbage input
while {1} {
    update
    set s [read $fp 512]
    if {$s eq ""} break
    puts stderr [format {Discarded %d bytes initial input} [string length $s]]
    after 250
}

# send our setup commands.  send them twice just to make sure.
puts $fp $cmds
flush $fp
after 100
update
puts $fp $cmds
flush $fp
after 100

# and consume the "echo" from those
while {1} {
    update
    set s [read $fp 512]
    if {$s eq ""} break
    puts stderr [format {Discarded %d bytes initial input} [string length $s]]
    after 100
}

# Now, get a report every 10 seconds or so for about $runsec seconds.
# $reports will hold all the reports; five list elements each:
#       + host time in milliseconds
#       + runs of the video IRQ
#       + matching events
#       + cpu cycles with interrupts enabled
#       + cpu cycles with interrupts disabled
set reports [list]

while {$runsec > 0 || [llength $reports] == 0} {
    if {[llength $reports] > 0} {
        # wait for our next one
        set wait 10
        if {$wait > $runsec} {
            set wait $runsec
        }
        puts stderr "Waiting $wait seconds"
        after [expr {$wait * 1000}]
        set runsec [expr {$runsec - $wait}]
    }

    # tell tst9video.asm to give us a report
    puts stderr "Now for a report"
    puts -nonewline $fp [format {%c[23|} 27]
    flush $fp

    # read the response
    set gotone 0
    update
    set line [string trim [gets $fp]]
    foreach wait {100 150 250 400 500 800 1000 0} {
        update
        set line [string trim [gets $fp]]
        if {$line eq ""} {
            # no response yet
        } elseif {[scan $line S%x,M%x,E%x,D%x gots gotm gote gotd] >= 4} {
            # valid response
            set gotone 1
            lappend reports \
                [clock milliseconds] \
                $gotm $gote $gotd
            break
        } else {
            # bogus response, or garbage before the response
            puts stderr "Ignoring [string length $line] char garbage line:[safestring $line]"
        }
        after $wait
    }
    if {!$gotone} {
        error "Timed out with no valid response."
    }
}

close $fp

# Print semi raw data
puts stderr "Reports:"
foreach {repms repm repe repd} $reports {
    puts stderr "\tTime $repms milliseconds"
    puts stderr "\t\tMatching events - $repm"
    puts stderr "\t\tClocks with IRQ enabled - $repe"
    puts stderr "\t\tClocks with IRQ disabled - $repd"
}

# Summarize the results of the reports, including where wraparound may
# have occurred.
set erste 1
set totms 0
set totm 0
set tote 0
set totd 0
foreach {repms repm repe repd} $reports {
    if {!$erste} {
        set totms [expr {$totms + entier($repms - $orepms)}]
        set totm [expr {$totm + entier(($repm - $orepm)&0x7fffffff)}]
        set tote [expr {$tote + entier(($repe - $orepe)&0x7fffffff)}]
        set totd [expr {$totd + entier(($repd - $orepd)&0x7fffffff)}]
    }
    set orepms $repms
    set orepm $repm
    set orepe $repe
    set orepd $repd
    set erste 0
}

puts stderr [format {Host-measured time: %f sec} [expr {double($totms)/1000}]]
puts stderr [format {Target clock cycles: %s} [expr {$tote + $totd}]]
puts stderr [format {Target-measured time: %f sec} \
    [expr {double($tote + $totd) / $targmhz / 1000000.0 }]]
puts stderr [format {Interrupt load: %f%%} \
    [expr {100 * double($totd) / double($tote + $totd)}]]
puts stderr [format {Matching runs: %s} $totm]
puts stderr [format {Matching runs per host second: %f} \
    [expr {double($totm) * 1000.0 / double($totms)}]]
puts stderr [format {Matching runs per target second: %f} \
    [expr {double($totm) * $targmhz * 1000000.0 / double($tote + $totd)}]]
puts stderr [format {Target clocks per matching run: %f} \
    [expr {double($tote + $totd) / double($totm)}]]

exit 0

