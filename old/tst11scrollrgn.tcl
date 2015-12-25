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

# Test the parameter processing of the ^[[...r command to set scroll region.
# Works not by actually doing scroll but by checking scroll_top & scroll_bot
# in memory.

set dev /dev/ttyUSB1
set baud 115200
set textsz [lindex $argv 0]
if {$textsz eq ""} { set textsz 40 }
set count [lindex $argv 1]
if {$count eq ""} { set count 100 }
set seed [lindex $argv 2]

# XXX gro: targmhz, mexpr, thresh, runsec

puts stderr "dev = $dev"
puts stderr "baud = $baud"
puts stderr "textsz = $textsz"
puts stderr "count = $count"
puts stderr "seed = $seed"

set scroll_top_adr 16
set scroll_bot_adr 17
set unit_delay_ms [expr {30000.0 / $baud + 3.0}]

##### initialization

# connect to serial port
set fp [open $dev r+]
fconfigure $fp -buffering none -encoding binary -translation binary \
    -blocking 0 -mode ${baud},n,8,1

# initialize pseudo random number generation
if {$seed ne ""} {
    expr srand($seed)
}

# consume any garbage input
while {1} {
    update
    set s [read $fp 512]
    if {$s eq ""} break
    puts stderr [format {Discarded %d bytes initial input} [string length $s]]
    after 250
}

##### operation

# readmem: read memory from the device, one byte at the given address
proc readmem {addr} {
    global fp unit_delay_ms
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

# readrng: read scroll range from the device, list of two integer values
proc readrng {} {
    global scroll_top_adr scroll_bot_adr
    return [list \
        [expr {[readmem $scroll_top_adr] + 1}] \
        [expr {[readmem $scroll_bot_adr] + 1}]]
}

# simulate: given a list of input integers, figure out what the result
# would be expected to produce in terms of top and bottom, as a list; or
# return empty list if it would be an error
proc simulate {parms} {
    global textsz
    lassign $parms ptop pbot
    if {$ptop eq ""} { set ptop 1 }
    if {$pbot eq ""} { set pbot $textsz }
    if {$pbot == 0} { set pbot $textsz }
    if {$ptop < 1} { set ptop 1 }
    if {$pbot > $textsz} { set pbot $textsz }
    if {$ptop > $textsz} { return [list] }
    if {$ptop >= $pbot} { return [list] }
    return [list $ptop $pbot]
}

# get the range so we can compare later
set oldrng [readrng]

while {$count > 0} {
    # decide what input to give
    set parms [list]
    switch -- [expr {int(rand()*20)}] {
        0 { set nparms 0 }
        1 -
        2 -
        3 -
        4 -
        5 -
        6 -
        7 { set nparms 1 }
        8 -
        9 -
        10 -
        11 -
        12 -
        13 -
        14 -
        15 { set nparms 2 }
        16 -
        17 -
        18 { set nparms 3 }
        19 { set nparms 4 }
    }
    if {rand() < 0.2} {
        set nparms 0
    } elseif {rand() < 0.4} {
        set nparms 1
    } elseif {rand() < 0.8} {
        set nparms 2
    } else {
        set nparms 3
    }
    set parms [list]
    for {set i 0} {$i < $nparms} {incr i} {
        switch -- [expr {int(rand()*10)}] {
            0 { lappend parms "" }
            1 { lappend parms [expr {int(rand()*65536)}] }
            2 { lappend parms [expr {int(rand()*768)}] }
            3 -
            4 { lappend parms [expr {int(rand()*256)}] }
            default { lappend parms [expr {int(rand()*($textsz + 4))}] }
        }
    }

    # issue the command
    set cmd [format {%c[%sr} 27 [join $parms ";"]]
    puts -nonewline $fp $cmd
    update
    after [expr {int($unit_delay_ms * [string length $cmd])}]

    # retrieve new range
    set newrng [readrng]

    # display & check results
    puts stderr "[format {^[[%sr} [join $parms ";"]] -> $newrng"
    set exprng [simulate $parms]
    if {![llength $exprng]} {
        set exprng $oldrng
    }
    if {$newrng ne $exprng} {
        puts stderr "Wrong result: expected $exprng"
        exit 1
    }

    # move on
    set oldrng $newrng
    incr count -1
}

exit 0

