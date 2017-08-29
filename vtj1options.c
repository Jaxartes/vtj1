/*
 * vtj1options.c - Set some terminal options on the VTJ-1 terminal.
 * Probably not as portable as I'd like, but compiles and seems to run ok
 * under both Linux and NetBSD 7.
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

static char *progname = "vtj1options";

static void
usage(void)
{
    fprintf(stderr, "%s: sets VTJ-1 terminal specific options\n"
            "USAGE: %s [OPTIONS]\n"
            "OPTIONS for VTJ-1 specific modes:\n"
            "\t-x/+x - don't/do show checkerboard glyph on some errors\n"
            "\t-b/+b - don't/do show cursor as block instead of underline\n"
            "\t-v/+v - don't/do flash screen to indicate bell\n"
            "\t-a/+a - don't/do make sound to indicate bell\n"
            "OPTIONS for modes defined by DEC historically:\n"
            "\t-c/+c - disable/enable cursor keys mode (DECCKM)\n"
            "\t-r/+r - disable/enable screenwise reverse video (DECSCNM)\n"
            "\t-o/+o - disable/enable origin mode (DECOM)\n"
            "\t-w/+w - disable/enable auto wrap mode (DECAWM)\n"
            /* leaving out DECARM since it doesn't work on my keyboard */
            "OPTIONS for modes defined by ANSI:\n"
            "\t-l/+l - unlock/lock keyboard\n"
            "\t-i/+i - disable/enable insert mode\n"
            "\t-e/+e - disable/enable \"no local echo\" mode\n"
            "\t-n/+n - disable/enable line feed / new line mode\n"
            "\t        +n - transmit both line feed and carriage return\n"
            "\t        -n - transmit only line feed or carriage return\n"
            "OPTIONS to control %s operation:\n"
            "\t-F - force operation, don't check for a VTJ-1 first\n"
            "\t-D dev - use device 'dev' instead of NULL; example: /dev/ttyUSB1\n",
            progname, progname, progname);
    exit(1);
}

/* mode settings identified by +char or -char and sent to the terminal
 * as ^[[...l (for -) or ^[[...h (for +).
 */
struct {
    char ch;
    char *seq;
    int chosen; /* 0 if not chosen, '+' or '-' if chosen */
} modes1[] = {
    { 'x', "=1", 0 },
    { 'b', "=3", 0 },
    { 'v', "=4", 0 },
    { 'a', "=5", 0 },
    { 'c', "?1", 0 },
    { 'r', "?5", 0 },
    { 'o', "?6", 0 },
    { 'w', "?7", 0 },
    { 'l', "2", 0 },
    { 'i', "4", 0 },
    { 'e', "12", 0 },
    { 'n', "20", 0 },
    { -1, NULL, 0 }
};

static void dumb_putchar(int fd, int ch);

int
main(int argc, char **argv)
{
    int i, j, a, ifd, ofd, none = 1;
    int optff = 0;
    char *optdd = NULL;

    /* figure out what we're called */
    if (argv && argv[0]) progname = argv[0];

    /* process any command line arguments; not using getopt() due to
     * the quirky +/- char syntax
     */
    for (a = 1; a < argc; ++a) {
        if (argv[a][0] == '-' && argv[a][1] == 'F' && argv[a][2] == '\0') {
            optff = 1;
        } else if (argv[a][0] == '-' && argv[a][1] == 'D' &&
                   argv[a][2] == '\0' && a + 1 < argc) {
            a++;
            optdd = argv[a];
        } else if ((argv[a][0] == '+' || argv[a][0] == '-') &&
                   argv[a][1] != '\0' && argv[a][2] == '\0') {
            for (i = 0; modes1[i].seq; ++i) {
                if (modes1[i].ch == argv[a][1]) {
                    break;
                }
            }
            if (modes1[i].seq) {
                modes1[i].chosen = argv[a][0];
                none = 0;
            } else {
                fprintf(stderr, "Unrecognized option character\r\n");
                usage();
            }
        } else {
            fprintf(stderr, "Unrecognized command line argument\r\n");
            usage();
        }
    }

    /* open the terminal */
    if (optdd == NULL) {
        ifd = STDIN_FILENO;
        ofd = STDOUT_FILENO;
    } else {
        ifd = open(optdd, O_RDONLY);
        if (ifd < 0) {
            perror(optdd);
            exit(1);
        }
        ofd = open(optdd, O_WRONLY);
        if (ofd < 0) {
            perror(optdd);
            exit(1);
        }
    }
    if (none) {
        fprintf(stderr, "%s: not doing anything, no options specified\r\n",
                progname);
        usage();
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

    /* issue the commands */
    for (i = 0; modes1[i].seq; ++i) {
        if (modes1[i].chosen) {
            dumb_putchar(ofd, 27);
            dumb_putchar(ofd, '[');
            for (j = 0; modes1[i].seq[j]; ++j)
                dumb_putchar(ofd, modes1[i].seq[j]);
            dumb_putchar(ofd, (modes1[i].chosen == '+' ? 'h' : 'l'));
        }
    }

    /* all done */
    return(0);
}

/*
 * dumb_putchar() - not very good but good enough for this purpose
 */
static void
dumb_putchar(int fd, int ch)
{
    char buf[1];
    int rv;

    buf[0] = ch;
    for (;;) {
        rv = write(fd, buf, 1);
        if (rv < 0) {
            if (errno == EAGAIN || errno == EINTR) {
                continue; /* retry */
            } else {
                return; /* fail */
            }
        } else {
            return; /* succeed or fail, either way: done */
        }
    }
}
