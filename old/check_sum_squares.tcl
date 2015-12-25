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

# check_sum_squares.tcl
# Takes input of the form:
#   0^2 + 0^2 = 0
#   1^2 + 0^2 = 1
#   1^2 + 1^2 = 2
#   2^2 + 0^2 = 4
# and so on.  Analyzes it, checking it for ordering, formatting, and
# correct results.  Reports any variations from the intended pattern.

set lastx ""
set lasty ""
set where "start"

while {![eof stdin]} {
    set line [string trimright [gets stdin]]
    set parts [split $line "^ "]
    if {[llength $parts] != 7} {
        puts stderr "Martian line after $where"
        set lastx ""
        set lasty ""
        set where "martian"
        continue
    }
    lassign $parts p0 p1 p2 p3 p4 p5 p6
    if {$p1 ne "2" || $p2 ne "+" || $p4 ne "2" || $p5 ne "=" ||
        ![string is entier -strict $p0] ||
        ![string is entier -strict $p3] ||
        ![string is entier -strict $p6]} {
        puts stderr "Martian line after $where"
        set lastx ""
        set lasty ""
        set where "martian"
        continue
    }
    set x $p0
    set y $p3
    set x2y2 $p6
    if {$lastx eq ""} {
        puts stderr "Sequence starts at $x^2 + $y^2 after $where"
    } elseif {$x == $lastx && $y == $lasty+1 && $y <= $x} {
        # normal: be quiet
    } elseif {$x == $lastx+1 && $y == 0 && $lasty == $lastx} {
        # normal: be quiet
    } else {
        puts stderr "Out of sequence $x^2 + $y^2 after $where"
    }
    if {$x*$x + $y*$y != $x2y2} {
        puts stderr "Incorrect value for $x^2 + $y^2, $x2y2, after $where"
    }
    set lastx $x
    set lasty $y

    set where "$x^2 + $y^2"
}

puts stderr "Ends after $where"
exit 0

