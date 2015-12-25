// Copyright (c) 2015 Jeremy Dilatush
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY JEREMY DILATUSH AND CONTRIBUTORS
// ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
// TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
// PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL JEREMY DILATUSH OR CONTRIBUTORS
// BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

// brg.v
// Baud rate generator for serial devices and other uses in the "VTJ-1"
// project.  Technically, this doesn't generate the baud rate, just extract
// it, but it was called BRG in my original design and I'm going to stick
// with the name.

module brg(
    input clk, // system clock: rising edge active
    input rst, // system reset: active-high synchronous
    input [13:0] masterbaud, // pulse trains for baud rate generation
            // masterbaud[0] has single-cycle pulses, 7,372,800 per second
            // masterbaud[1] has pulses half that often
            // ...
            // masterbaud[13] has 900 per second
    input [4:0] ctl, // 5-bit control register which configures the baud rate
            // ctl[3:0] - power of 2 to divide masterbaud/8 by, up to 10
            // ctl[4] - divide that in turn by 3
            //      baud        ctl
            //      230.4k      0x02
            //      115.2k      0x03
            //      9600        0x15
            //      300         0x1a
    output baudout // single clock cycle pulses at the given rate
);
    // BAUD8: if this is zero, then 'baudout' is at the given baud rate
    //        if this is one, then 'baudout' is eight times that fast
    parameter BAUD8 = 1'b0;

    // Select baud rate or baud rate times 3 from masterbaud[]
    reg sel = 1'b0;
    always @(posedge clk)
        if (rst)
            sel <= 1'b0;
        else if (BAUD8) case(ctl[3:0])
        4'd0: sel <= masterbaud[0];
        4'd1: sel <= masterbaud[1];
        4'd2: sel <= masterbaud[2];
        4'd3: sel <= masterbaud[3];
        4'd4: sel <= masterbaud[4];
        4'd5: sel <= masterbaud[5];
        4'd6: sel <= masterbaud[6];
        4'd7: sel <= masterbaud[7];
        4'd8: sel <= masterbaud[8];
        4'd9: sel <= masterbaud[9];
        default: sel <= masterbaud[10]; // 10
        endcase else case (ctl[3:0])
        4'd0: sel <= masterbaud[3];
        4'd1: sel <= masterbaud[4];
        4'd2: sel <= masterbaud[5];
        4'd3: sel <= masterbaud[6];
        4'd4: sel <= masterbaud[7];
        4'd5: sel <= masterbaud[8];
        4'd6: sel <= masterbaud[9];
        4'd7: sel <= masterbaud[10];
        4'd8: sel <= masterbaud[11];
        4'd9: sel <= masterbaud[12];
        default: sel <= masterbaud[13]; // 10
        endcase

    // Optionally divide 'sel' by 3 to get the baud rate.
    reg [2:0]thr = 3'd1;
    always @(posedge clk)
        if (rst)
            thr <= 3'd1;
        else if (sel)
            thr <= { thr[1:0], thr[2] };

    // And that'll give us our result
    assign baudout = sel && (thr[0] || !ctl[4]);
endmodule
