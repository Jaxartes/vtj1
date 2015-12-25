#!/usr/bin/tclsh
# Copyright (C) 2015 Jeremy Dilatush
# Feel free to use this code in any project (commercial or not), as long as you
# keep this message, and the copyright notice. This code is provided "as is", 
# without any warranties of any kind. 

# vtj1vers.tcl
# This script generates a short description of what version of the VTJ-1
# software this is.  Mostly it's a build identifier.  The description is
# written as "vtj1vers.asm" replacing the existing contents of that file.
# Expects to be run in the same directory with the other VTJ-1 sources
# right before building "vtj1.asm".

# settings
set version 1.0 ; # coarse version number
set srcfile "vtj1.asm" ; # file to read
set dstfile "vtj1vers.asm" ; # file to write

# collect information
set hash [exec openssl md5 -r $srcfile | cut -c1-8]
if {[info exists env(USER)]} {
    set user $env(USER)
    puts stderr "Got user name from environment"
} elseif {![catch {exec id -un} user]} {
    puts stderr "Got user name from 'id'"
} else {
    puts stderr "Unable to discover user name"
    set user ""
}
set date [clock format [clock seconds] -gmt 1 -format {%Y-%m-%d %H:%M:%SZ}]

# open file & write it
set dfp [open $dstfile w]
set msg "$version $hash $date $user"
set msg2 ""
foreach ch [split $msg {}] {
    if {[string is print $ch] && $ch ne "'" && $ch ne "\"" && $ch ne "\\"} {
        append msg2 $ch
    }
}
puts $dfp "; Version information for use in vtj1.asm"
puts $dfp "; Automatically generated by [info script]"
puts $dfp "; Do not archive or edit"
puts $dfp "asc \"$msg2\""
close $dfp
puts stderr "Have put version string in $dstfile:\n\"$msg2\""
exit 0