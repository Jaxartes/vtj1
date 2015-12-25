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

# tst18termid.tcl
# Test the 'Device Attributes (DA)' escape sequence and other escape
# sequences that cause the terminal to write something back.
# Expects the terminal to be in stdin & stdout, already set up properly.
# Displays the characters it gave back.

set seqdb(da) [list "Device Attributes (DA)" "\033\[c"]
set seqdb(da2) [list "Device Attributes (DA) variant" "\033\[0c"]
set seqdb(decid) [list "Identify Terminal (DECID)" "\033Z"]
set seqdb(dsr) [list "Device Status Report (DSR)" "\033\[5n"]
set seqdb(psr) [list "Printer status report" "\033\[?15n"]
set seqdb(cpr) [list "Cursor Position Report (CPR)" "\033\[6n"]
set seqdb(-) [list "Do nothing" ""]

fconfigure stdin -translation binary -encoding binary -buffering none

set seqname [lindex $argv 0]
if {[info exists seqdb($seqname)]} {
    lassign $seqdb($seqname) seqlabel seqreal
    puts stderr "Using sequence $seqname: $seqlabel"
} elseif {[string match "data:*" $seqname]} {
    set seqlabel "Raw data"
    set seqreal [string range $seqname 5 end]
} else {
    puts stderr "Unknown sequence name \"$seqname\"."
    puts stderr "You may specify a sequence with data:..."
    puts stderr "or use one of the following:"
    foreach seqname [lsort [array names seqdb]] {
        puts stderr [format "\t%-8s %s" $seqname [lindex $seqdb($seqname) 0]]
    }
    exit 1
}

exec stty raw -echo

# consume input for 2 seconds
set b [fconfigure stdin -blocking]
fconfigure stdin -blocking 0
after 2000
update ; update ; update
set got [read stdin 8192]
puts stderr "Ignoring [string length $got] bytes received early."

# issue the sequence
puts stderr "Issuing the sequence."
puts -nonewline stdout $seqreal
flush stdout

# consume input for 2 seconds
after 2000
update
set got [read stdin 8192]
fconfigure stdin -blocking 1

puts stderr "Received [string length $got] bytes back."
for {set i 0} {$i < [string length $got]} {incr i 16} {
    set sub [string range $got $i ${i}+15]
    # characters if printable
    set s ""
    for {set j 0} {$j < [string length $sub]} {incr j} {
        set char [string index $got ${i}+${j}]
        if {[string is print $char]} {
            append s " ${char}  "
        } else {
            append s "    "
        }
    }
    puts stderr $s

    # octal values
    set s ""
    for {set j 0} {$j < [string length $sub]} {incr j} {
        set byte [scan [string index $got ${i}+${j}] %c]
        append s [format "%03o " $byte]
    }
    puts stderr $s

    # hex values
    set s ""
    for {set j 0} {$j < [string length $sub]} {incr j} {
        set byte [scan [string index $got ${i}+${j}] %c]
        append s [format " %02x " $byte]
    }
    puts stderr $s
    puts stderr ""
}

exec stty sane
exit 0
