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

# This script connects to the VTJ-1 software, and uses the "developer"
# escape sequences to load a small "bootloader" program into screen memory.
# Then it executes it to load a new version of the VTJ-1 software.

proc usage {} {
    puts stderr {
        USAGE: tclsh reload.tcl [dev=$dev] [baud=$baud] < srecs
            dev=$dev: device; default /dev/ttyUSB1
            baud=$baud: baud rate; default 115200
            bl=$srecs: file name of bootloader program; default reload.srec
            verbose: turn on extra verbose output
            readbackbl: read back the code from reload.srec & check it that way
            noload: start the assembly code and then exit
    }
    exit 1
}

# system parameters
set rom_rw {0xb282 10} ; # addr-value to set "ROM" to read-write
set timeout_ms 200 ; # short for a human, long enough for serial port
set blram 0x8000

# command line arguments
set dev /dev/ttyUSB1
set baud 115200
set blfile reload.srec
set opt_verbose 0
set opt_readbackbl 0
set opt_ugly 0
set opt_noload 0

foreach arg $argv {
    set nv [split $arg =]
    lassign $nv n v
    if {$arg eq "verbose"} {
        incr opt_verbose
    } elseif {$arg eq "readbackbl"} {
        set opt_readbackbl 1
    } elseif {$arg eq "ugly"} {
        # ugly, and unreliable, but maybe easier to debug
        set opt_ugly 1
    } elseif {$n eq "noload"} {
        set opt_noload 1
    } elseif {$n eq "dev" && $v ne ""} {
        set dev $v
    } elseif {$n eq "baud" && [string is integer -strict $v]} {
        set baud $v
    } elseif {$n eq "bl" && $v ne ""} {
        set blfile $v
    } else {
        puts stderr "Argument '$arg' not understood"
        usage
    }
}

puts stderr "Parameter dev=$dev"
puts stderr "Parameter baud=$baud"
puts stderr "Parameter bl=$blfile"

# Code to read S-records from a file.  This gets run twice, once for $blfile
# and once (from stdin) for the *real* program.  This only understands
# "S1" records.  Those are data records with two byte addresses.  There's no
# need for longer addresses; an assembler for the 6502 shouldn't generate
# them.  The assembler I'm using (crasm) also generates S9 records, which
# purportedly can contain the address of the starting instruction, but at
# least when assembling my programs it's always zero.  So instead, I'll put
# the start address in memory at 0xfffc, which is what I'd be doing if
# I put these in "ROM" anyway.

# The data it read will be returned in a big long list, alternating address
# and data values.

proc readsrec {fp lbl} {
    global opt_verbose
    if {$opt_verbose} {
        puts stderr "$lbl: Beginning"
    }
    set lc_blank 0 ; # skipped lines: blank
    set lc_nons 0 ; # skipped lines: no "S"
    set lc_parse 0 ; # skipped lines: basic parse error, or unknown type
    set lc_nop 0 ; # skipped lines: a type we do nothing with
    set lc_use 0 ; # lines not skipped
    set alldata [list] ; # data that's been received
    while {![eof $fp]} {
        # get a line
        set line [string trim [gets $fp]]
        if {$line eq ""} {
            # skip this line
            incr lc_blank
            continue
        }
        if {[string index $line 0] ne "S"} {
            # skip this line
            incr lc_nons
            continue
        }

        # parse it
        set pl [parse1 $line]

        if {![llength $pl]} {
            # skip this line
            incr lc_parse
            continue
        }

        lassign $pl Type Addr Data

        # handle it
        switch -- $Type {
            S1 {
                # Data record
                set l [llength $Data]
                if {$Addr + $l > 65536 || $Addr < 0} {
                    error "Address out of range: Record addr $Addr len $l"
                }
                for {set i 0} {$i < $l} {incr i} {
                    lappend alldata [expr {$Addr + $i}] [lindex $Data $i]
                }
                incr lc_use
            }
            default {
                # skip this line
                incr lc_nop
                continue
            }
        }
    }

    if {$opt_verbose} {
        puts stderr "$lbl: Done."
    }

    # Print a report on what we got
    if {$opt_verbose} {
        foreach {rl rv} [list \
            "Skipped lines (blank)" $lc_blank \
            "Skipped lines (not 'S')" $lc_nons \
            "Skipped lines (parse/type)" $lc_parse \
            "Skipped lines (ignored)" $lc_nop \
            "Used lines" $lc_use \
        ] {
            puts stderr [format {%s: %27s %s} $lbl $rl $rv]
        }
    }

    # Return what we got
    return $alldata
}

# parse1 - pass 1 of parsing an S-Record.  Breaks it into pieces, returned
# in a list:
#       type (including "S")
#       address
#       data
# it validates the checksum & doesn't return it.  The address value it
# returns is a single numeric value; the data value it returns is a
# list of numeric byte values.
#
# Returns empty list on recoverable parse error or a type of record that
# is ignored.
proc parse1 {sr} {
    set l [string length $sr]
    if {$l < 2} {
        error "Record is truncated in its type field"
    }
    set type [string range $sr 0 1]
    if {$l < 4} {
        error "$type record is truncated in its record length field"
    }
    set reclen [scan [string range $sr 2 3] %x]
    switch -- $type {
        S0 { set alen 4 }
        S1 { set alen 4 }
        S2 { set alen 6 }
        S3 { set alen 8 }
        S5 { set alen 4 }
        S6 { set alen 6 }
        S7 { set alen 8 }
        S8 { set alen 6 }
        S9 { set alen 4 }
        default { return [list] }
    }
    if {$l < $alen + 4} {
        error "$type record is truncated in its address part"
    }
    set addr [scan [string range $sr 4 $alen+3] %x]
    set reclend [expr {$reclen * 2 + 4}]
    if {$l != $reclen * 2 + 4} {
        error \
            "$type record has wrong length, encoded $reclend ($reclen) real $l"
    }

    set cksum [scan [string range $sr end-1 end] %x]
    set cksum2 0
    for {set i 2} {$i < $l - 2} {incr i 2} {
        set byte [scan [string range $sr $i $i+1] %x]
        set cksum2 [expr {($cksum2 + $byte) & 255}]
    }
    set cksum2 [expr {$cksum2 ^ 255}]
    if {$cksum != $cksum2} {
        error "$type record bad checksum, got $cksum exp $cksum2"
    }

    set data [list]
    for {set i [expr {$alen + 4}]} {$i < ($l - 2)} {incr i 2} {
        lappend data [scan [string range $sr $i $i+1] %x]
    }

    return [list $type $addr $data]
}

# Read the two files of S-records

if {[catch {open $blfile r} blfp]} {
    error "Failed to open $blfile: $blfp"
}
set bldata [readsrec $blfp "Reading $blfile"]
if {[llength $bldata] < 32} {
    error "$blfile is way too short, what's wrong?"
} elseif {[llength $bldata] > 8192} {
    error "$blfile is way too long, what's wrong?"
}
foreach {a d} $bldata {
    if {$a < $blram || $a >= $blram + 4096} {
        error "$blfile contains out of range address $a, what's wrong?"
    }
}

set realdata [readsrec stdin "Reading S-recs from stdin"]
if {[llength $realdata] < 32} {
    puts stderr "S-records on stdin way too few, will skip execution of $blfile"
    set realdata [list]
}

# Open the serial port.
puts stderr "Connecting to serial port $dev"
set fp [open $dev r+]
fconfigure $fp -buffering none -encoding binary -translation binary -blocking 1
fconfigure $fp -handshake xonxoff
fconfigure $fp -mode ${baud},n,8,1

if {!$opt_ugly} {
    # The "bootloader" is going to be stored in video memory, so let's not
    # show it, or have cursor blinking or other screen updates interfere
    # with it.
    set linctl_a 576 ; # should match the value in vtj1.asm
    set rowcnt 51 ; # number of rows in video memory
    set saferow 32 ; # where the rows we don't need are
    for {set r 0} {$r < $rowcnt} {incr r} {
        puts -nonewline $fp \
            [format {%c[92;%u;%u|} 27 \
                [expr {$linctl_a + $r}] \
                [expr {$saferow + (($r == 0) ? 0 : (($r == $rowcnt - 1) ? 2 : 1))}]]
    }
    puts -nonewline $fp "\033\[r\033\[m\033\[H\033\[2JReloading..."
}

# Poke the "bootloader" into screen memory byte by
# byte using developer escape sequences.  Do it three times for the sake
# of reliability.
foreach time {first second third} {
    puts stderr "Poking $blfile's data into memory, $time time out of three"
    foreach {a d} $bldata {
        puts -nonewline $fp [format {%c[92;%u;%u|} 27 $a $d]
        puts -nonewline stderr .
    }
    puts stderr "Done."
}

if {$opt_readbackbl} {
    puts stderr "Trying to read $blfile's data back from memory"
    fconfigure $fp -blocking 0
    set extra 1000 ; # allow some extra delays at first
    foreach {a d} $bldata {
        puts -nonewline $fp [format {%c[69;%u|} 27 $a]
        set tstart [clock milliseconds]
        set got ""
        while {([clock milliseconds] - $tstart) < ($timeout_ms + $extra) &&
               [string length $got] < 2} {
            append got [read $fp [expr {2 - [string length $got]}]]
            after [expr {int(min($timeout_ms / 10, 5))}]
        }
        if {[string length $got] < 2} {
            error "Timeout during readback, aborting"
        }
        set d2 [scan $got %02x]
        if {$d2 != $d} {
            error "Bad data ($d2) at $a, expected $d"
        }
        puts -nonewline stderr .
        set extra 0
    }
    puts stderr "Done."
    fconfigure $fp -blocking 1
}

# Now make the "ROM" writeable
puts -nonewline $fp \
    [format {%c[92;%u;%u|} 27 [lindex $rom_rw 0] [lindex $rom_rw 1]]

# If there's no data to load, that's all we do
if {![llength $realdata] && !$opt_noload} {
    # There is no data to load.  Exit early.
    exit 0
}

# Prepare to load the data.

puts stderr "Preparing to load data."

# This escape sequence is entered very conservatively to reduce the risk
# of serial port problems, because there's no failsafe.
set cmd [format {%c%c%c[253;%u|} 24 24 27 $blram]
if {$opt_verbose} {
    puts stderr "Issuing command to jump to $blram"
}
foreach cmdchar [split $cmd {}] {
    puts -nonewline $fp $cmdchar
    after 50
}
fconfigure $fp -handshake none
after $timeout_ms

# Put the data in array form.
for {set i 0} {$i < 65536} {incr i} {
    set mem($i) 0
}
foreach {a d} $realdata {
    set mem($a) $d
    set page([expr {$a >> 8}]) 1
}

if {$opt_verbose} {
    puts stderr "Found pages:"
    foreach p [lsort -integer [array names page]] {
        puts -nonewline stderr " $p"
    }
    puts stderr ""
}

if {$opt_noload} {
    exit 0
}

# Now write, page by page.

proc emit_addr {a} {
    global fp opt_verbose
    if {$opt_verbose > 1} {
        puts stderr [list emit_addr $a]
    }
    puts -nonewline $fp [format %c%c%c%c \
        [expr {(($a >> 12) & 15) | 32}] \
        [expr {(($a >> 8 ) & 15) | 32}] \
        [expr {(($a >> 4 ) & 15) | 32}] \
        [expr {(($a      ) & 15) | 32}]]
}

proc emit_byte {b} {
    global fp opt_verbose
    if {$opt_verbose > 2} {
        puts stderr [list emit_byte $b]
    }
    puts -nonewline $fp [format %c%c \
        [expr {$b & 15}] \
        [expr {(($b >> 4) & 15) | 16}]]
}

proc snarf_garbage {} {
    global fp opt_verbose
    while {1} {
        set got [read $fp 512]
        if {$got eq ""} { return }
        if {$opt_verbose} {
            puts stderr "snarf_garbage got [string length $got] bytes"
        }
        after 25
    }
}

proc snarf_addr {} {
    global fp timeout_ms opt_verbose
    if {$opt_verbose > 1} {
        puts stderr "snarf_addr running"
    }
    fconfigure $fp -blocking 0
    while {1} {
        snarf_garbage
        puts -nonewline $fp [format %c 150]
        set tstart [clock milliseconds]
        set got ""
        while {[string length $got] < 4 &&
               ([clock milliseconds] - $tstart) < $timeout_ms} {
            append got [read $fp [expr {4 - [string length $got]}]]
            after [expr {int(min($timeout_ms / 10, 5))}]
        }
        if {[string length $got] >= 4} {
            # The address is in modified hexadecimal.
            if {[string length $got] != 4} {
                puts stderr "Weird data received: wrong number of digits."
                puts stderr "Trying again."
                continue
            }
            set a 0
            for {set i 0} {$i < 4} {incr i} {
                set cc [scan [string index $got $i] %c]
                if {$cc < 64 || $cc >= 80} {
                    puts stderr "Weird data received: Bad byte $cc."
                    puts stderr "Trying again."
                    continue
                }
                set a [expr {($a << 4) | ($cc & 15)}]
            }
            fconfigure $fp -blocking 1
            return $a
        } else {
            puts stderr "Timeout trying to read an address, trying again."
            continue
        }
    }
}

foreach p [lsort -integer [array names page]] {
    # Figure out the page
    set a [expr {$p * 256}]
    if {$opt_verbose} {
        puts stderr [format "Writing page 0x%04x" $a]
    } else {
        puts -nonewline stderr [format "(%02x" [expr {$a >> 8}]]
    }

    while {1} {
        # Set address
        while {1} {
            emit_addr $a

            # Verify address
            set a2 [snarf_addr]
            if {$a2 == $a} {
                break
            }
            puts stderr "Error setting address, trying again."
        }

        # Write page data while computing check code
        set ck 0
        set a1 $a
        for {set i 0} {$i < 256} {incr i} {
            emit_byte $mem($a1)
            # update the check code for this byte
            set ck [expr {$ck + $mem($a1)}]
            set ck [expr {($ck & 0xffff) + ($ck >> 16)}]
            set ck [expr {(($ck & 0x7fff) << 1) + ($ck >> 15)}]
            # next byte
            incr a1
        }

        # Check that: by checking address and check code
        set a2 [snarf_addr]
        if {(($a + 256) & 0xffff) != $a2} {
            puts stderr [format "Error writing page 0x%04x, retrying" $a]
            continue
        }
        emit_addr $a
        puts -nonewline $fp [format %c 200]
        set ck2 [snarf_addr]
        if {$ck != $ck2} {
            if {$opt_verbose} {
                puts stderr \
                    [format "check code: page 0x%04x, got 0x%04x, exp 0x%04x" \
                        $a $ck2 $ck]
            }
            puts stderr [format "Error (cc) writing page 0x%04x, retrying" $a]
            continue
        }
        break
    }
    if {$opt_verbose} {
        puts stderr [format "Wrote page 0x%04x" $a]
    } else {
        puts -nonewline stderr ")"
    }
}
puts stderr ""

# Done writing the data.  Now execute.
puts stderr "Done writing data.  Now writing \"done counter\" bytes."
puts stderr \
    "These may appear onscreen, perhaps as accented \"u\" or checkerboard."
set dcb [format %c 250]
for {set i 0} {$i < 10} {incr i} {
    puts -nonewline $fp [string repeat $dcb 20]
    after 50
}
puts -nonewline $fp [string repeat $dcb 100]
close $fp
puts stderr "Done."

exit 0
