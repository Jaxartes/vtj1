# Makefile: used with 'make' to build stuff in VTJ-1
# As yet it does not build the whole thing, just some parts.

# build: build all the stuff (except the parts which are built outside the
# Makefile)
build: config rom.mem font.mem reload.srec

# config: creates three files, vtj1-config.{v,asm,cooked}, from vtj1-config.txt.
config:
	tclsh vtj1-config-gen.tcl

# clean: Remove some generated files.
clean:
	-rm vtj1-config.v vtj1-config.asm vtj1-config.cooked
	-rm vtj1.srec reload.srec rom.mem font.mem vtj1.bits

# vtj1.srec: assemble the software part.  Including version information.
# Grumble, grumble, "crasm" doesn't seem to give a nonzero exit status
# to indicate error.  And doesn't seem to honor its own -x option.  The
# 'egrep' command included below mitigates both problems, sort of.
# (Still has a problem with error / success getting backwards due to
# the semantics of 'grep'.)
vtj1.srec: vtj1.asm vtj1-config.asm
	./vtj1vers.tcl
	-crasm -l -x -o vtj1.srec vtj1.asm | \
		egrep '^(ERRORS|WARNINGS): *[1-9]|^>+ +[0-9]+ ERROR:|Abs END_OF_CODE'

reload.srec: reload.asm
	-crasm -o reload.srec reload.asm | \
		egrep '^(ERRORS|WARNINGS): *[1-9]|^>+ +[0-9]+ ERROR:|Abs END_OF_CODE'

# rom.mem: Program memory contents.  It's generated
# from vtj1.srec, and in turn used as input to the FPGA synthesis tools
# to generate the block RAM which is used as the "ROM"
rom.mem: vtj1.srec vtj1-config.cooked
	tclsh srec2memh.tcl `grep '^srec2memh_args=' vtj1-config.cooked | \
            cut -d= -f2-` \
            < vtj1.srec > rom.mem

# font.mem: Font memory contents.  It's generated using vtj1fonter.tcl
# from various inputs, some included with VTJ-1, and some separate.
font.mem: vtj1-config.cooked
	tclsh vtj1fonter.tcl `grep '^fonter_args=' vtj1-config.cooked | \
            cut -d= -f2-` out=font.mem
