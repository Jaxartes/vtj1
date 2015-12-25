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

# Test case program for cursor movement escape sequences in VTJ-1.  It
# draws a grid on the screen, the vertical lines being made of characters,
# the horizontal by colors.  The character cells are drawn in pseudorandom
# order.  Each is drawn twice, the first time wrong, the second time right.
# Navigation between the character positions is by various combinations of
# escape sequences.  The settings of origin mode and scroll region, which
# determine much of these sequences' behavior, is varied too.

# YYY:
# The behavior of this test script is not perfect.  Sometimes it leaves gaps
# and I haven't found out why.  It's good enough to test VTJ-1, which has
# passed it, and which is the purpose.

# The general idea of a broad random variation in operations intended to
# produce a predictable result, like this, is better in theory than in
# practice.  The two chief problems are that the test tends to have its
# own bugs, and that debugging any problems (found in the test or by it)
# is made difficult by the unpredictable operations.

# openings

set dev /dev/ttyUSB1
set baud 115200
set seed [lindex $argv 0]
set width 80
set textsz 40
set linger 0
set reset 0
foreach mod [lrange $argv 1 end] {
    if {$mod eq "stdout"} {
        # instead of using the serial port, just go to stdout
        set dev ""
    } elseif {$mod eq "linger"} {
        # after finishing, wait around a while; useful with 'stdout'
        set linger 1
    } elseif {$mod eq "reset"} {
        # reset terminal before starting
        set reset 1
    } elseif {$mod eq "turbo"} {
        # speed up, by transmitting as fast as possible and leaving it
        # to the OS to do flow control.
        # This is already the default.  The non-"turbo" case stopped working.
    } else {
        puts stderr "Unknown keyword $mod, ignored."
    }
}

puts stderr "dev = $dev"
puts stderr "baud = $baud"
puts stderr "seed = $seed"
puts stderr "textsz = $textsz"

# pattern
set colors [list 40 44 41] ; # black blue red
set chars [list {.} {|} {@}] ; # light medium heavy

if {$dev eq ""} {
    set fp stdout
    set baud 115200
} else {
    set fp [open $dev r+]
    fconfigure $fp -buffering none -encoding binary -translation binary \
        -blocking 1 -mode ${baud},n,8,1 \
        -handshake xonxoff
}

# Predictable pseudo random number generation. (Using MINSTD.)
set prngs [expr {147036 + 258147 * $seed}]
proc prng {} {
    global prngs
    set prngs [expr {($prngs * 48271) % 0x7fffffff}]
    return $prngs
}

# Gradual transmission of data.
proc emit {s} {
    global fp baud
    foreach b [split $s ""] {
        puts -nonewline $fp $b
        flush $fp
    }
}

# Decide on the order in which the character cells are to be written; each
# twice.  The list $order will contain a list for each write, with two
# entries, the 1-based Y and X coordinates.
set preorder [list]
foreach _ {_ _} {
    for {set x 1} {$x <= $width} {incr x} {
        for {set y 1} {$y <= $textsz} {incr y} {
            lappend preorder [list [prng] $y $x]
        }
    }
}
set order [list]
foreach ety [lsort -integer -index 0 $preorder] {
    lappend order [lrange $ety 1 2]
}

# Reset if desired.
if {$reset} {
    emit [format %cc 27]
    if {$dev ne ""} {
        after 15000
    }
}

# Home position, full-screen scroll, and clear the screen
emit [format {%c[m%c[r%c[2J} 27 27 27]

# Now write them.  The array wronged() keeps track of ones we've written the
# wrong character to already.
array unset wronged
set color "" ; # color we're currently in
set cur_org 0 ; # whether origin mode is currently enabled
set cur_top 1 ; # current top of scroll region
set cur_bot $textsz ; # current bottom of scroll region
set deferred_move 0 ; # deferred cursor move after char at column 80
set x 1 ; # current position
set y 1 ; # current position

set writectr 0
foreach cell $order {
    # figure out where we're going to draw now
    lassign $cell cy cx

    # figure out what the right color & character are for it
    set right_color $cy
    set right_char $cx

    # figure out what we're going to draw, right or wrong
    if {[info exists wronged($cell)]} {
        set draw_color $right_color
        set draw_char [lindex $chars [expr {$right_char % [llength $chars]}]]
    } else {
        set wronged($cell) 1
        set draw_color [expr {$cy + 1 + ([prng] % 2)}]
        set draw_char [format %c [expr {97+([prng] % 26)}]]
    }

    # convert that to the actual codes
    set draw_color [lindex $colors [expr {$draw_color % [llength $colors]}]]

    # move to position
    while {$x != $cx || $y != $cy || $deferred_move} {
        # pseudorandom choice of how we're going to try to move
        set choice [expr {[prng] % 490}] ; # 490 = 5 * 49 * 2
        if {!($choice % 5)} {
            # intentionally push boundary
            set ipbd ""
            if {$x == $cx && $x == 1} {
                # At left edge; if we try to move further left
                # we go nowhere.  Which is where we want to go.
                set ipbd D ; # left
            }
            if {$x == $cx && $x == $width} {
                # At right edge; if we try to move further right
                # we go nowhere.  Which is where we want to go.
                set ipbd C ; # right
            }
            if {$y == $cy && $y == $cur_top} {
                # At top edge; if we try to move further up
                # we go nowhere.  Which is where we want to go.
                set ipbd A ; # up
            }
            if {$y == $cy && $y == $cur_bot} {
                # At bottom edge; if we try to move further down
                # we go nowhere.  Which is where we want to go.
                set ipbd A ; # down
            }
            if {$ipbd ne ""} {
                set ipba [expr {1 + ([prng] % 10)}]
                if {!([prng] % 50)} {
                    set ipba [expr {10 + ([prng] % 10000)}]
                }
                if {$ipba == 1} {
                    if {[prng] % 2} {
                        set ipba ""
                    }
                }
                emit [format {%c[%s%s} 27 $ipba $ipbd]
            }
        }
        if {$choice % 49} {
            # Actually move, if possible.  First, decide whether to use "CUP"
            # or the directional cursor movements.
            set cup [expr {$choice % 2}]
            if {$cup} {
                # Using CUP or its equivalent HVP to jump to the specified
                # place.
                if {$cur_org && ($cy < $cur_top || $cy > $cur_bot)} {
                    # Can't do it, we're in origin mode and it's outside
                    # the scroll region.
                    continue
                } else {
                    set cupcode [expr {([prng] % 2) ? "f" : "H"}]
                    set cupx $cx
                    if {$cupx == 1} {
                        switch -- [expr {[prng] % 5}] {
                            0 { set cupx 0 }
                            1 { set cupx "" }
                        }
                    } elseif {$cupx == $width} {
                        switch -- [expr {[prng] % 5}] {
                            0 {set cupx [expr {$width + 1 + ([prng] % 20)}] }
                            1 {set cupx [expr {$width + 1 + ([prng] % 500)}]}
                        }
                    }
                    set cupy $cy
                    set maxy $textsz
                    if {$cur_org} {
                        set cupy [expr {$cy - $cur_top + 1}]
                        set maxy [expr {$cur_bot - $cur_top + 1}]
                    }
                    if {$cupy == 1} {
                        switch -- [expr {[prng] % 5}] {
                            0 { set cupy 0 }
                            1 { set cupy "" }
                        }
                    } elseif {$cupy == $maxy} {
                        switch -- [expr {[prng] % 5}] {
                            0 { set cupy [expr {$maxy + 1 + ([prng] % 20)}] }
                            1 { set cupy [expr {$maxy + 1 + ([prng] % 500)}]}
                        }
                    }

                    # Build the CUP command.
                    set s [format {%c[} 27]
                    if {$cupx eq "" && $cupy eq "" && ([prng] % 2)} {
                        # leave out the parameters
                    } elseif {$cupx eq "" && ([prng] % 3)} {
                        # leave out one parameter
                        append s $cupy
                    } else {
                        # include both parameters
                        append s "${cupy};${cupx}"
                        # and maybe more
                        if {!([prng] % 10)} {
                            append s [format {;%d} [expr {[prng] % 20}]]
                        }
                        if {!([prng] % 15)} {
                            append s [format {;%d} [expr {[prng] % 500}]]
                        }
                    }
                    append s $cupcode

                    # Now emit the CUP command.
                    emit $s
                    set x $cx
                    set y $cy
                    set deferred_move 0
                }
            } else {
                # Using CUU/CUD/CUF/CUB to move cursor to position if possible.
                if {$cy < $cur_top && $y >= $cur_top} {
                    # can't get there, out of the scroll region, this way
                    continue
                }
                if {$cy > $cur_bot && $y <= $cur_bot} {
                    # can't get there, out of the scroll region, this way
                    continue
                }
                # Now move in one more more steps.
                while {$cx != $x || $cy != $y} {
                    # See which ways we can move without getting further
                    # from our goal.
                    set mvn [expr {max($y - $cy, 0)}]
                    set mvs [expr {max($cy - $y, 0)}]
                    set mve [expr {max($cx - $x, 0)}]
                    set mvw [expr {max($x - $cx, 0)}]

                    # Pick a movement.
                    if {($mvn || $mvs) && ($mve || $mvw)} {
                        if {[prng] % 2} {
                            set mvn 0
                            set mvs 0
                        } else {
                            set mve 0
                            set mvw 0
                        }
                    }
                    if {$mvn} { set mvd A ; set mva $mvn } \
                    elseif {$mvs} { set mvd B ; set mva $mvs } \
                    elseif {$mve} { set mvd C ; set mva $mve } \
                    elseif {$mvw} { set mvd D ; set mva $mvw } \
                    else {
                        # Shouldn't happen, but oh well.
                        error "no valid move"
                    }

                    # Pick an amount, we might or might not move the
                    # whole way.
                    switch -- [expr {[prng] % 3}] {
                        0 { set mva 1 }
                        1 { set mva [expr {1 + ([prng] % $mva)}] }
                    }

                    # Build the command
                    set s [format {%c[} 27]
                    if {$mva > 1 || ([prng] % 2)} {
                        if {$mva > 1 || ([prng] % 5)} {
                            append s $mva
                        } else {
                            # equivalent
                            append s 0
                        }
                        if {!([prng] % 10)} {
                            # bogus parameter to be ignored
                            append s [format {;%d} [expr {[prng] % 20}]]
                        }
                        if {!([prng] % 15)} {
                            # bogus parameter to be ignored
                            append s [format {;%d} [expr {[prng] % 500}]]
                        }
                    } else {
                        # leave the parameter blank
                    }
                    append s $mvd

                    # Emit the command
                    emit $s

                    # Adjust the cursor position based on what we just did.
                    switch -- $mvd {
                        A { set y [expr {$y - $mva}] }
                        B { set y [expr {$y + $mva}] }
                        C { set x [expr {$x + $mva}] }
                        D { set x [expr {$x - $mva}] }
                    }
                    set deferred_move 0
                }
            }
            continue
        }
        if {$choice % 2} {
            # change scroll region setting
            set cur_top 1
            set cur_bot $textsz
            set s [format {%c[} 27]
            if {[prng] % 5} {
                set cur_bot 1
                while {$cur_top >= $cur_bot} {
                    set cur_top 1
                    set cur_bot $textsz
                    if {[prng] % 3} {
                        set cur_top [expr {1 + ([prng] % ($textsz - 1))}]
                    }
                    if {[prng] % 3} {
                        set cur_bot [expr {2 + ([prng] % ($textsz - 1))}]
                    }
                }
                if {$cur_top == 1 && $cur_bot == $textsz & ([prng] % 2)} {
                    append s ""
                } elseif {$cur_bot == $textsz && ([prng] % 2)} {
                    append s $cur_top
                } else {
                    append s "${cur_top};${cur_bot}"
                    if {!([prng] % 10)} {
                        append s ";[expr {[prng] % 1023}]"
                    }
                    if {!([prng] % 15)} {
                        append s ";[expr {[prng] % 1023}]"
                    }
                }
            }
            emit ${s}r
            set x 1
            set y [expr {$cur_org ? $cur_top : 1}]
            set deferred_move 0
            continue
        }
        if {1} {
            # change origin mode setting
            if {$cur_org} {
                # was on, now make it off
                set cur_org 0
                emit [format {%c[?6l} 27]
                set x 1
                set y 1
                set deferred_move 0
            } else {
                # was off, now make it on
                set cur_org 1
                emit [format {%c[?6h} 27]
                set x 1
                set y $cur_top
                set deferred_move 0
            }
            continue
        }
    }

    # change color if desired
    if {$draw_color ne $color} {
        emit [format {%c[%sm} 27 $draw_color]
    }

    # draw character
    emit $draw_char
    incr writectr

    # adjust position for the drawn character
    if { $x < $width } {
        incr x
        set deferred_move 0
    } else {
        set deferred_move 1
    }
}

# done, now clear up our state somewhat

emit [format {%c[m} 27]
if {$linger} {
    after 300000
}

exit 0
