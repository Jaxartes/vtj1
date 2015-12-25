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

# vtj1-config-gen.tcl
# This script reads "vtj1-config.txt", containing VTJ-1 configuration
# settings, and generates various files which control the build of
# the various parts of VTJ-1.

set input_filename vtj1-config.txt
set output_filename_a vtj1-config.asm
set output_filename_v vtj1-config.v
set output_filename_c vtj1-config.cooked

# Open vtj1-config.txt, and read and parse it into an array, $cfg(...)
set fp [open ${input_filename} r]
while {![eof $fp]} {
    # Get a line
    set line [gets $fp]
    # Deal with comments and whitespace
    set hash [string first "#" $line]
    if {$hash > 0} {
        set line [string range $line 0 [expr {$hash-1}]]
    } elseif {$hash == 0} {
        continue ; # empty line
    }
    set line [string trim $line]
    # If it's a blank line do nothing
    if {$line eq ""} continue
    # Now it should be name=value
    set equal [string first "=" $line]
    if {$equal < 0} {
        puts stderr "Problem in line read from ${input_filename}:"
        puts stderr "Not a blank line, a comment, or a name=value pair"
        puts stderr "Line is: $line"
        error "Bad input"
    } else {
        set name [string range $line 0 [expr {$equal-1}]]
        set value [string range $line [expr {$equal+1}] end]
        set name [string trim $name]
        set value [string trim $value]
        if {[info exists cfg($name)]} {
            puts stderr "Problem in line read from ${input_filename}:"
            puts stderr "Name '$name' appears in more than one line"
            error "Bad input"
        } else {
            set cfg($name) $value
        }
    }
}
close $fp
puts stderr "Got [llength [array names cfg]] settings from ${input_filename}."

# Now go through the settings and fill in default values for the ones
# that have been left out, and check the rest.
array set defaults {
    writeable_rom 0
    big_rom 0
    writeable_font 0
    big_ram 0
    font_256_chars 0
    video_timing_tester 0
    visible_bell 1
    audible_bell 1
    devel_escapes 1
    devel_escapes_2 0
    progdelay 0
    bufhwm 0
    menu 1
    menu_lowbauds 0
    pgalign 0
    baudrate 115200
    clock_rate 75
    fpga_platform papilio_pro_arcade
    font lat1-12-vt
}
foreach {n dv} [array get defaults] {
    if {![info exists cfg($n)]} {
        # Value not given, fill in a default
        set cfg($n) $dv
    }
}
foreach {n v} [array get cfg] {
    if {![info exists defaults($n)]} {
        # This is not a known setting.
        puts stderr "Unknown name '$n' in ${input_filename}."
        error "Bad input"
    } elseif {$n eq "fpga_platform"} {
        if {$v ne $defaults($n)} {
            puts stderr "Unsupported value for fpga_platform= ($v)"
            puts stderr "Currently the only supported value is $defaults($n)."
            puts stderr "To support another platform will require code"
            puts stderr "changes and plenty of testing."
            error "Bad input"
        }
    } elseif {$n eq "font"} {
        if {![string match "*-vt" $cfg($n)]} {
            puts stderr "Warning: Font setting '$cfg($n)' may fail to"
            puts stderr "         work properly, since it looks like it"
            puts stderr "         isn't one of the '-vt' variants.  Consider"
            puts stderr "         trying a different font."
            # Not an error, just a warning
        }
        foreach ch [split $cfg($n) ""] {
            if {!($ch eq "-" || [string is alnum $ch])} {
                puts stderr \
                    "Illegal font setting '$cfg($n)'; contains potentially"
                puts stderr "troublesome characters."
                error "Bad input"
            }
        }
    } elseif {![string is integer -strict $v]} {
        puts stderr "Value is not an integer in ${n}=${v} in ${input_filename}."
        error "Bad input"
    }
}

# And some more complicated cross checks.
# In the following list, there are two entries for each dependency:
#       name of a setting which depends on another
#       name of the setting it depends on
foreach {dependent dependency} {
    video_timing_tester devel_escapes
    devel_escapes_2 devel_escapes
    progdelay devel_escapes
    bufhwm devel_escapes
    video_timing_tester devel_escapes_2
    menu_lowbauds menu
} {
    if {$cfg($dependent) && !$cfg($dependency)} {
        puts stderr "Warning: ${dependent}=1 is not very useful without"
        puts stderr "         ${dependency}=1."
    }
}

# including baud rate; see brg.v for the code which interprets this value.
set magicbaud 921600
for {set i 0} {$i <= 10} {incr i} {
    for {set j 0} {$j < 32} {incr j 16} {
        set b [expr {$magicbaud >> $i}]
        if {$j} { set b [expr {int($b / 3)}] }
        set baudcodes($b) [expr {$i + $j}]
    }
}
if {[info exists baudcodes($cfg(baudrate))]} {
    set baudcode $baudcodes($cfg(baudrate))
} else {
    puts stderr "Baud rate $cfg(baudrate) is not a supported baud rate"
    puts stderr "(read from ${input_filename}).  Available:"
    foreach b [lsort -int [array names baudcodes]] {
        puts stderr "\t${b} baud"
    }
    error "Bad input"
}

# and clock rate
# known_clock_rates:
#       for each clock rate: label, MHz, multiple of 25, list of `defines
set known_clock_rates {
    25 25.0   1 {VTJ1_CLOCK_25MHZ VTJ1_VIDEO_25MHZ}
    50 50.0   2 VTJ1_CLOCK_50MHZ
    51 51.2   2 VTJ1_CLOCK_51200KHZ
    75 74.667 3 VTJ1_CLOCK_74667KHZ
    77 76.8   3 VTJ1_CLOCK_76800KHZ
}
set clock_rate_ok 0
set clock_rate_avail [list]
foreach {crlbl crmhz crper crdef} $known_clock_rates {
    if {$cfg(clock_rate) eq $crlbl} {
        set clock_rate_ok 1
    }
    lappend clock_rate_avail $crlbl
}
if {!$clock_rate_ok} {
    puts stderr "clock_rate setting in ${input_filename}, $cfg(clock_rate),"
    puts stderr "not accepted.  Must be one of the following:"
    puts stderr "\t$clock_rate_avail"
    error "Bad input"
}

# Now that that's all been checked, time to write the output files.
set afp [open ${output_filename_a} w]
set vfp [open ${output_filename_v} w]
set cfp [open ${output_filename_c} w]

set timestamp [clock format [clock seconds]]
foreach {fp cmt} [list $afp ";" $vfp "//" $cfp "\#" ] {
    puts $fp "${cmt} DO NOT EDIT THIS FILE."
    puts $fp "${cmt} IF YOU DO, YOUR EDITS MAY BE DELETED."
    puts $fp "${cmt} Instead, edit ${input_filename},"
    puts $fp "${cmt} from which this file is generated."
    if {[info script] ne ""} {
        puts $fp "${cmt} Conversion performed by [info script]"
    }
    puts $fp "${cmt} Conversion performed at $timestamp"
    puts $fp ""
}

foreach n [lsort [array names defaults]] {
    puts $cfp [format {%s=%s} $n $cfg($n)]
}

foreach {on in} {
    WRITEABLE_ROM writeable_rom
    BIG_ROM big_rom
    WRITEABLE_FONT writeable_font
    BIG_RAM big_ram
    FONT_256_CHARS font_256_chars
    VIDEO_TIMING_TESTER video_timing_tester
} {
    if {$cfg($in)} {
        puts $vfp "`define $on // Derived from '${in}=1'"
    } else {
        puts $vfp "// Not defining $on, due to '${in}=0'"
    }
}
foreach {on in} {
    ENABLE_VISBELL visible_bell
    ENABLE_AUDBELL audible_bell
    ENABLE_DEVEL devel_escapes
    ENABLE_DEVEL_2 devel_escapes_2
    ENABLE_PROGDELAY progdelay
    ENABLE_BUFHWM bufhwm
    ENABLE_MENU menu
    ENABLE_MENU_LOWBAUDS menu_lowbauds
    ENABLE_BIGROM big_rom
    ENABLE_PGALIGN pgalign
} {
    puts $afp "${on} = $cfg($in) ; from setting '$in'"
}

# handle clock rate and things which depend on it
puts $vfp "`define XILINX_SPARTAN_6 // assumed FPGA hardware type"
puts $vfp "`define INPUT_CLOCK_32MHZ // assumed clock, as on Papilio board"
foreach {crlbl crmhz crper crdef} $known_clock_rates {
    if {$cfg(clock_rate) eq $crlbl} {
        set mhz $crmhz ; # clock rate in MHz, to use in calculations
        set per $crper ; # clock cycles per pixel
        foreach def $crdef {
            puts $vfp [format {`define %s // clock_rate=%s (%.3f MHz)} \
                $def $cfg(clock_rate) $mhz]
        }
        break
    }
}

# MASTERBAUD_RATE is a `define that controls the master baud rate pulse
# train of 7,372,800 pulses per second, as follows:
#       MASTERBAUD_RATE = 483183.8208 / system_clock_MHz
set masterbaud_rate [expr {round(483183.8208 / $mhz)}]
if {$masterbaud_rate < 100} {
    error "Current clock rate would produce MASTERBAUD_RATE = $masterbaud_rate, too low."
}
if {$masterbaud_rate > 65535} {
    error "Current clock rate would produce MASTEBAUD_RATE = $masterbaud_rate, too high."
}
puts $vfp [format {`define MASTERBAUD_RATE 16'd%u // depends on clock rate, produces 7,372,800 Hz} $masterbaud_rate]

# baud rate setting
puts $afp \
    [format {DEFAULT_BAUDRATE_CODE = %u ; %u baud, set from vtj1-config.txt} \
        $baudcode $cfg(baudrate)]

# dividing the system clock rate down to 25MHz for the pixel clock
puts $vfp \
    [format {`define PIXEL_ENABLE_RATE %u // clock rate over 25MHz, to control pixel rate} $per]
if {$per == 1} {
    puts $vfp \
        [format {`define PIXEL_ENABLE_RATE_1 // clock rate is 25MHz}]
}

# some convenience arguments for use at build time
puts $cfp "# srec2memh_args is autogenerated depending on big_rom="
puts $cfp [format {srec2memh_args=sz=%u of=%u} \
    [expr {$cfg(big_rom) ? 16384 : 8192}] \
    [expr {$cfg(big_rom) ? 49152 : 57344}]]
puts $cfp \
    "# fonter_args is autogenerated depending on font= and font_256_chars="
puts $cfp [format {fonter_args=font_256_chars=%u fnt=%s} \
    [expr {$cfg(font_256_chars) ? 1 : 0}] $cfg(font)]

close $afp
close $vfp
close $cfp
puts stderr "Generated ${output_filename_a} based on ${input_filename}"
puts stderr "Generated ${output_filename_v} based on ${input_filename}"
puts stderr "Generated ${output_filename_c} based on ${input_filename}"
exit 0

