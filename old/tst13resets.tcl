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

# Test case for resetting the terminal; there's been occasional trouble with
# it failing to resume video output.  I think I've gotten it fixed, but it's
# worth running more test cases than are convenient to do manually.

# This script does the following:
#       + issues two random and at least one valid escape sequences
#       + prints some text
#       + resets terminal
#       + waits 20 seconds
#       + checks video output state
# It relies on some of the "developer" escape sequences.

# openings

set dev /dev/ttyUSB1
set baud 115200
set count [lindex $argv 0]
set pause 0
set fakefail 0
foreach mod [lrange $argv 1 end] {
    if {$mod eq "stdout"} {
        set dev ""
    } elseif {$mod eq "pause"} {
        set pause 1
    } elseif {$mod eq "fakefail"} {
        set fakefail 1
    } else {
        puts stderr "Unknown keyword $mod, ignored."
    }
}

if {$dev eq ""} {
    set fp stdout
    set baud 115200
} else {
    set fp [open $dev r+]
    fconfigure $fp -buffering none -encoding binary -translation binary \
        -blocking 0 -mode ${baud},n,8,1
}

puts stderr "dev = $dev"
puts stderr "baud = $baud"
puts stderr "count = $count"

# Gradual transmission of data.
proc emit {s} {
    global fp baud
    foreach b [split $s ""] {
        puts -nonewline $fp $b
        flush $fp
        after [expr {int(ceil(25000 / $baud))}]
    }
}

# Emit a random escape sequence
proc randesc {} {
    # start sequence with ^[[
    set s [format {%c[} 27]
    # add some parameters, maybe
    if {rand() < 0.2} {
        append s "?"
    }
    set parms [list]
    if {rand() < 0.4} {
        while {rand() < 0.5} {
            if {rand() < 0.3} {
                lappend parms ""
            } else {
                lappend parms [expr {int(rand()*(1<<(int(rand()*15))))}]
            }
        }
    }
    append s [join $parms {;}]
    # add intermediate bytes, maybe
    if {rand() < 0.2} {
        while {rand() < 0.5} {
            append s [format %c [expr {int(rand()*16+32)}]]
        }
    }
    emit $s
}

# readmem: read memory from the device, one byte at the given address
proc readmem {addr} {
    global fp baud
    set unit_delay_ms [expr {30000.0 / $baud + 3.0}]
    set backoff 1
    while {1} { ; # repeat until done
        # pick pseudorandom integers to echo before & after
        set pre [expr {int(rand()*65536)}]
        set post [expr {int(rand()*65536)}]

        # make it transmit three integers in hex: $pre, the value, $post
        set cmd [format {%c[115;%u|%c[69;%u|%c[115;%u|} \
            27 $pre 27 $addr 27 $post]
        puts -nonewline $fp $cmd

        # give it a chance to respond
        update
        after [expr {int($unit_delay_ms * ([string length $cmd] + 10))}]
        update
        set s [read $fp 512]

        # parse the result
        if {[string length $s] < 10} {
            puts stderr "readmem: missing output, retrying"
            after $backoff
            set backoff [expr {min(int($backoff*1.25),1000)}]
            continue
        }
        lassign [scan $s {%04x%02x%04x%n}] gpre gval gpost glen
        if {$glen eq "" || $glen < 10} {
            puts stderr "readmem: parse error, retrying"
            after $backoff
            set backoff [expr {min(int($backoff*1.25),1000)}]
            continue
        }
        if {$glen > 10} {
            puts stderr "readmem: [expr {$glen - 10}] bytes garbage ignored"
        }
        if {$gpre != $pre || $gpost != $post} {
            puts stderr "readmem: pre/post value error, retrying"
            puts stderr "(got $gpre $gpost exp $pre $post)"
            after $backoff
            set backoff [expr {min(int($backoff*1.25),1000)}]
            continue
        }
        return $gval
    }
}

# Perform a single test cycle; returns 1 if ok 0 if not
proc doit {} {
    global pause fakefail

    # put something on the screen
    randesc
    randesc
    if {rand() < 0.5} {
        emit [format {%c[r%c[J} 27 27]
    }
    if {rand() < 0.5} {
        emit [format {%c#6} 27]
    }
    set fg [expr {int(rand()*8)}]
    set bg [expr {int(rand()*7+1)^$fg}]
    emit [format {%c[;3%d;4%dm} 27 $fg $bg]
    for {set i 0} {$i < (rand()*10+5)} {incr i} {
        emit [format %c [expr {int(rand()*26)+int(rand()*2)*32+65}]]
    }

    # pause if desired
    if {$pause} {
        after 5000
    }

    # reset the terminal
    emit [format {%cc} 27]

    # wait 20 seconds to give the terminal plenty of time to come up again
    after 20000

    if {$fakefail && rand() < 0.4} {
        # cause video failure in order to test that we detect it
        emit [format {%c[92;%d;%d|} 27 0xb100 0x40]
        after 5000
    }

    # check video output state
    set res [expr {[readmem 45314] & 15}]
    if {$res == 0} {
        return 1
    } else {
        return 0
    }
}

set ctr_ok 0
set ctr_fail 0
for {set i 0} {$i < $count} {incr i} {
    set res [doit]
    puts stderr "Rep ${i}: [expr {$res ? "ok" : "fail"}]"
    incr [expr {$res ? "ctr_ok" : "ctr_fail"}]
}

puts stderr "Did $i reps, $ctr_ok ok, $ctr_fail fail."

exit 0
