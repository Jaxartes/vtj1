/*
 * vtj1_login_init.c
 * Apply a bunch of settings and workarounds for using VTJ-1 as a login
 * terminal.  Meant for Linux.  May be portable to similar systems.  Maybe.
 * 
 * XXX unfinished
 *
 * Copyright (c) 2017 Jeremy Dilatush
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY JEREMY DILATUSH AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL JEREMY DILATUSH OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

extern int vtj1check(int i, int o); /* in vtj1check.c */

static char *progname = "vtj1_login_init";

static void
usage(void)
{
    fprintf(stderr, "%s: sets up your session for using a VTJ-1 terminal\n"
            "USAGE: %s [OPTIONS]\n"
            "OPTIONS:\n"
            "\t-p - skip allocating a PTY for the TIOCSCTTY workaround\n"
            "\t-l - skip setting non-UTF-8 $LANG value\n"
            "\t-t - skip trying to set $TERM\n"
            "\t-T - skip checking terminfo availability before setting TERM=vtj1\n"
            "\t-e - skip 'stty erase ^H'\n"
            "\t-F - don't check if terminal is VTJ-1, just assume it is\n",
            progname, progname);
    exit(1);
}

int
main(int argc, char **argv)
{
    int oc;
    int ifd, ofd;

    int optff = 0;
    int optp = 0, optl = 0, optt = 0, opttt = 0, opte = 0;

    char iname[128], oname[128];

    /* figure out what we're called */
    if (argv && argv[0]) progname = argv[0];

    /* process any command line arguments */
    while ((oc = getopt(argc, argv, "pltTeF")) != -1) {
        switch (oc) {
        case 'p': optp = 1; break;
        case 'l': optl = 1; break;
        case 't': optt = 1; break;
        case 'T': opttt = 1; break;
        case 'e': opte = 1; break;
        case 'F': optff = 1; break;
        default:
            fprintf(stderr, "Unrecognized option\n");
            usage();
        }
    }
    if (optind < argc) {
        fprintf(stderr, "Unrecognized argument\n");
        usage();
    }

    /* check whether stdin & stdout are a terminal & set up to use them*/
    ifd = STDIN_FILENO;
    ofd = STDOUT_FILENO;
    if (!isatty(ifd)) {
        fprintf(stderr, "%s: stdin is not a tty, giving up\n", progname);
        exit(1);
    }
    if (!isatty(ofd)) {
        fprintf(stderr, "%s: stdout is not a tty, giving up\n", progname);
        exit(1);
    }
    if (ttyname_r(ifd, iname, sizeof(iname))) {
        snprintf(iname, sizeof(iname), "(unknown terminal name - stdin)\n");
        exit(1);
    }
    if (ttyname_r(ofd, oname, sizeof(oname))) {
        snprintf(iname, sizeof(iname), "(unknown terminal name - stdout)\n");
        exit(1);
    }
    if (strcmp(iname, oname) != 0) {
        fprintf(stderr, "%s: stdout and stdin are different TTYs (%s / %s), giving up\n", progname, iname, oname);
        exit(1);
    }

    /* check whether it's a VTJ-1, unless the user gave us -F */
    if (!optff) {
        if (!vtj1check(ifd, ofd)) {
            fprintf(stderr, "\r\n"
                    "%s: Apparently terminal is not a VTJ-1; aborting\r\n"
                    "This check may be suppressed with the -F option\r\n",
                    progname);
            exit(1);
        }
    }

    /* if desired, allocate a PTY to work around the fact that TIOCSCTTY
     * doesn't work on some drivers I've encountered
     */
    if (optp) {
        /* -p option -- user wants to skip this step */
    } else {
        /* XXX not sure how to do this */
    }

    /* XXX */
}
