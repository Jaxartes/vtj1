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

# companion to tst9video.asm
# takes one or more reports and prints out the results
# in CSV decimal integers, in the following order:
#   number of times the IRQ routine ran
#   current scan line
#   number of times the criterion was matched
#   number of clock cycles with interrupts enabled
#   number of clock cycles with interrupts disabled
# Run with the following two parameters:
#   Number of samples
#   Interval between them in milliseconds

set dev /dev/ttyUSB1
set baud 115200
set nsamp [lindex $argv 0]
set sampms [lindex $argv 1]

puts stderr "dev = $dev"
puts stderr "baud = $baud"
puts stderr "nsamp = $nsamp"
puts stderr "sampms = $sampms"

set fp [open $dev r+]
fconfigure $fp -buffering none -encoding binary -translation binary \
    -blocking 0 -mode ${baud},n,8,1

for {set i 0} {$i < $nsamp + 3} {incr i} {
    # a little delay
    if {$i < $nsamp} {
        after $sampms
    } elseif {$sampms < 333} {
        after 333
    } else {
        after $sampms
    }

    # request a sample
    if {$i < $nsamp} {
        puts $fp "r"
    }

    # look for any results we've gotten
    while {1} {
        update ; # run the event loop, where the actual reading happens
        set line [string trim [gets $fp]]
        if {$line ne ""} {
            # we got a line, try to parse it as our data
            if {[scan $line R%x,S%x,M%x,E%x,D%x gotr gots gotm gote gotd] >= 5} {
                # Matching line
                puts "$gotr,$gots,$gotm,$gote,$gotd"
                flush stdout
            } else {
                puts stderr "Ignoring garbage line."
            }
        }
        if {[fblocked $fp]} {
            # no more data, for now
            break
        }
    }
}
