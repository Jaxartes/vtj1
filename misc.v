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

// misc.v
// A collection of various small Verilog modules that don't merit having
// files of their own.

// dummyio()
// "dummy" I/O device for "VTJ-1" project.  This fills in an I/O slot
// (with 256 bytes of registers and 2 IRQ lines) that is not really used
// for anything.  It supplies discovery signals as per serial devices so
// that the software can know what to do with it.
// All it does is act like ROM filled with zeros.
module dummyio(
    // I/O device interface
    input clk, // system clock: rising edge active
    input rst, // system reset: active-high synchronous
    input [7:0] adr, // register address
    input [7:0] adr_d1, // 'adr' delayed by 1 clock cycle
    output [7:0] red, // data being read
    input [7:0] wrt, // data being written
    input wen, // enable write
    output irqa, // interrupt request ("alpha IRQ")
    output irqb  // interrupt request ("beta IRQ")

    // Device specific interface: none
);
    assign red = 8'd0;
    assign irqa = 1'b0, irqb = 1'b0;
endmodule

// prienc32()
// A priority encoder with 32 inputs.  Its output will be the number
// of the lowest numbered one that's a '1'.  If they're all '0' the output
// will be 63.
module prienc32(
    input [31:0]ins,
    output [5:0]out
);
    // The priority encoder is built up in stages.  We start with the
    // 32 input lines.  Then we pair them together to get 16x 2-input
    // encoders, then likewise for 8x 4-input ones, and so on.

    genvar g;

    // 16x 2-input encoders, each with 1-bit-plus-1 output
    wire [15:0]twos;
    wire [15:0]twosno;
    generate
        for (g = 0; g < 16; g = g + 1) begin : dotwos
            assign { twosno[g], twos[g] } =
                ins[2*g] ? 2'd0 : (ins[2*g+1] ? 2'd1 : 2'd3);
        end
    endgenerate

    // 8x 4-input encoders, each with 2-bit-plus-1 output
    wire [15:0] fours;
    wire [7:0] foursno;
    generate
        for (g = 0; g < 8; g = g + 1) begin : dofours
            assign foursno[g] = twosno[2*g+1] & twosno[2*g];
            assign fours[(2*g+1):(2*g)] =
                { twosno[2*g], twosno[2*g] ? twos[2*g+1]
                                           : twos[2*g] };
        end
    endgenerate

    // 4x 8-input encoders, each with 3-bit-plus-1 output
    wire [11:0] eights;
    wire [3:0] eightsno;
    generate
        for (g = 0; g < 4; g = g + 1) begin : doeights
            assign eightsno[g] = foursno[2*g+1] & foursno[2*g];
            assign eights[(3*g+2):(3*g)] =
                { foursno[2*g], foursno[2*g] ? fours[(4*g+3):(4*g+2)]
                                             : fours[(4*g+1):(4*g)] };
        end
    endgenerate

    // 2x 16-input encoders, each with 4-bit-plus-1 output
    wire [7:0] sixteens;
    wire [1:0] sixteensno;
    generate
        for (g = 0; g < 2; g = g + 1) begin : dosixteens
            assign sixteensno[g] = eightsno[2*g+1] & eightsno[2*g];
            assign sixteens[(4*g+3):(4*g)] =
                { eightsno[2*g], eightsno[2*g] ? eights[(6*g+5):(6*g+3)]
                                               : eights[(6*g+2):(6*g)] };
        end
    endgenerate

    // And finally, the 32-input encoder, with 5-bit-plus-1 output
    assign out[5] = sixteensno[1] & sixteensno[0];
    assign out[4:0] = { sixteensno[0], sixteensno[0] ? sixteens[7:4]
                                                     : sixteens[3:0] };
        
endmodule

`ifdef TEST_PRIENC32_IN_ICARUS_VERILOG
module prienc32_test();
    reg [31:0] state = 0;
    reg [39:0] count = 0;
    wire [5:0] enced;
    prienc32 e(.ins(state), .out(enced));
    initial forever begin
        // Show raw input and output.
        #1;
        $display("(%d) ins = %08x, out = %d", count, state, enced);

        // A simple check for correct result.
        #1;
        if (enced == 63) begin
            if (state == 0) begin
                // ok
            end else begin
                $display("ERR: encoded 63, nonzero state $%x", state);
                $finish;
            end
        end else if (enced >= 32) begin
            $display("ERR: encoded value %d", enced);
            $finish;
        end else if (state & ((1 << enced) - 1)) begin
            $display("ERR: encoded value %d too high for state $%x",
                     enced, state);
            $finish;
        end else if ((state >> enced) & 1) begin
            // ok
        end else begin
            $display("ERR: encoded value %d too low for state $%x",
                     enced, state);
            $finish;
        end

        // Next state: this is not an LFSR, but kind of like one
        #1;
        if (state)
            state <= { state[30:0], 1'b0 } ^
                     (state[31] ? 32'd65 : 32'd0);
        else
            state <= 1;
        count <= count + 1;
    end
endmodule
`endif

`ifdef TEST2_PRIENC32_IN_ICARUS_VERILOG
module prienc32_test2();
    reg [35:0] state = 0;
    wire [5:0] enced;
    prienc32 e(.ins(state[31:0]), .out(enced));
    reg [5:0] v, w, x, y, z, expect;
    initial begin
        for (w = 0; w < 36; w = w + 1) begin
            for (x = w + 1; x < 36; x = x + 1) begin
                for (y = x + 1; y < 36; y = y + 1) begin
                    for (z = y + 1; z < 36; z = z + 1) begin
                        state <= (1 << w) | (1 << x) | (1 << y) | (1 << z);
                        if (w >= 32)
                            expect <= 63;
                        else 
                            expect <= w;
                        #1;
                        $display("ins = %08x, out = %d", state[31:0], enced);
                        if (enced != expect) begin
                            $display("mismatch!");
                            $finish;
                        end
                    end
                end
            end
        end
    end
endmodule
`endif
