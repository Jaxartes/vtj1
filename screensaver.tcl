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

# screensaver.tcl: Silly little text mode screensaver

proc usage {} {
    puts stderr {Usage: tclsh screensaver.tcl [file [rows [columns]]]}
    puts stderr "\tfile - file name to go to, - for stdout, default stdout"
    puts stderr "\trows - number of rows, default 40"
    puts stderr "\tcols - number of columns, default 80"
    exit 1
}

if {[llength $argv] > 3 || [catch {
    set file [lindex $argv 0]
    set rows [lindex $argv 1]
    set cols [lindex $argv 2]

    if {$file eq "" || $file eq "-"} {
        set fp stdout
    } else {
        set fp [open $file w]
    }
    if {$rows eq ""} { set rows 40 }
    if {$cols eq ""} { set cols 80 }
} err]} {
    puts stderr "Error: $err"
    usage
}

set ncells [expr {$rows * $cols}]
set pos $ncells
set maxrun 100

while {1} {
    if {$pos >= $ncells} {
        # go back from lower right to upper left corner
        puts -nonewline $fp [format {%c[H} 27]
        set pos 0
    }
    set run [expr {min($ncells - $pos, int(rand()*$maxrun+1))}]
    switch [expr {int(rand()*3)}] {
        0 {
            # select character attributes (colors, etc)
            set ats [list]
            lappend ats "" ; # clear any previous settings
            if {rand() < 0.7} {
                # foreground & background colors
                set bg [expr {int(rand()*8)}]
                set fg 0
                while {!($fg && ($fg - 1))} {
                    set fg [expr {int(rand()*7+1)}]
                }
                set fg [expr {$bg^$fg}]
                lappend ats [expr {30+$fg}]
                lappend ats [expr {40+$bg}]
            }
            if {rand() < 0.3} { lappend ats 1 } ; # bold
            if {rand() < 0.3} { lappend ats 4 } ; # underline
            if {rand() < 0.3} { lappend ats 7  } ; # inverse
            puts -nonewline $fp [format {%c[%sm} 27 [join $ats ";"]]
        }
        1 {
            # display blanks
            puts -nonewline $fp [string repeat " " $run]
            incr pos $run
        }
        2 {
            # display text
            set s ""
            for {set i 0} {$i < $run} {incr i} {
                append s [format %c [expr {int(rand()*26)+97-((rand()<0.2) ? 32 : 0)}]]
            }
            puts -nonewline $fp $s
            incr pos $run
        }
    }

    flush $fp
    # just make it a little better behaved, linux seems to not like the
    # display stuff being just dumped sometimes
    after 1
}
