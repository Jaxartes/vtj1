/*
 * vtj1check.c - Check to see if the current terminal is a VTJ-1
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

#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/select.h>
#include <sys/time.h>
#include <termios.h>

static int timedout_getchar(int i);

/* vtj1check() - Check to see if the terminal (accessed via input file
 * descriptor 'i' and output file pointer 'o') is a VTJ1.  Returns 1 if so,
 * 0 if not or in case of error.
 */
int
vtj1check(int i, int o)
{
    const char *query = "\033[0;86c"; /* query string */
    const char *xresp1 = "=86;84;74;"; /* part of expected response */
    int p, rv, l, ch, ok_so_far = 1;
    struct termios tios, tios2;
    int tios_valid = 0;

    /* alter terminal state into non-canonical mode */
    memset(&tios, 0, sizeof(tios));
    if (tcgetattr(i, &tios) >= 0) tios_valid = 1;
    if (tios_valid) {
        tios2 = tios;
        tios2.c_lflag &= ~ICANON;
        tios2.c_cc[VMIN] = 1;
        tios2.c_cc[VTIME] = 2;
        tios2.c_lflag &= ~ECHO;
        tcsetattr(i, TCSANOW, &tios2);
    }

    /* send the query string to the terminal */
    l = strlen(query);
    for (p = 0; p < l; ) {
        rv = write(o, query + p, l - p);
        if (rv < 0) {
            if (errno == EAGAIN || errno == EINTR) {
                /* transient error, try again */
                continue;
            }
            ok_so_far = 0;
            goto vtj1check_done;
        } else if (rv == 0) {
            ok_so_far = 0;
            goto vtj1check_done;
        }
        p += rv;
    }

    /* Get a response, but don't wait forever.  Complicated by the need
     * to recognize a valid response that isn't the one we expected.
     * See also ECMA-48 section 5.4.
     */
    /* read CSI: 27 91 or 155 */
    ch = timedout_getchar(i);
    if (ch < 0) {
        ok_so_far = 0;
        goto vtj1check_done;
    }
    if (ch == 27) {
        ch = timedout_getchar(i);
        if (ch != 91) {
            ok_so_far = 0;
            goto vtj1check_done;
        }
    } else if (ch != 155) {
        ok_so_far = 0;
        goto vtj1check_done;
    }

    /* process one or more 'P' bytes (48-63); in our case this should start
     * with xresp1; 'p' counts position therein, and 'ok_so_far' keeps
     * track of whether we've seen anything very wrong.
     */
    p = 0;
    for (;;) {
        ch = timedout_getchar(i);
        if (ch < 0) {
            ok_so_far = 0;
            goto vtj1check_done;
        }
        if (ch < 48 || ch >= 64) break;
        if (xresp1[p] && xresp1[p] != ch) ok_so_far = 0;
        if (xresp1[p]) p++;
    }

    /* process zero or more 'I' bytes (32-47); not expecting any */
    for (;;) {
        if (ch < 32 || ch >= 48) break;
        ok_so_far = 0;
        ch = timedout_getchar(i);
        if (ch < 0) {
            goto vtj1check_done;
        }
    }

    /* process the 'F' byte (64-126); ours should be 'c' (99) */
    if (ch != 99) {
        ok_so_far = 0;
        goto vtj1check_done;
    }
vtj1check_done:
    if (tios_valid) {
        tios2 = tios;
        tcsetattr(i, TCSANOW, &tios2);
    }
    return(ok_so_far);
}

/* timedout_getchar() - read a character but don't wait forever */
static int
timedout_getchar(int i)
{
    struct timeval tv;
    fd_set rfds;
    int rv;
    unsigned char ch;

    for (;;) {
        /* The use of select() here may be unnecessary with the termios
         * settings made in vtj1check().
         */
        FD_ZERO(&rfds);
        FD_SET(i, &rfds);
        tv.tv_usec = 250000; /* time to wait without anything happening */
        tv.tv_sec = 0;
        rv = select(i + 1, &rfds, NULL, NULL, &tv);
        if (rv < 0) {
            if (errno == EINTR) {
                continue; /* try again */
            } else {
                return(-1); /* error */
            }
        } else if (rv == 0) {
            return(-1); /* timed out */
        } else {
            rv = read(i, &ch, sizeof(ch));
            if (rv < 0) {
                if (errno == EINTR || errno == EAGAIN) {
                    continue; /* try again */
                } else {
                    return(-1); /* error */
                }
            } else if (rv == 0) {
                return(-1); /* no more bytes to get */
            } else {
                return(ch); /* got it! */
            }
        }

        /* shouldn't be reached, but try to avoid an infinite loop */
        return(-1);
    }
}

