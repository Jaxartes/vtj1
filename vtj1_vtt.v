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

// vtj1_vtt.v
// I/O device for "VTJ-1" project for testing the video timings produced
// by and with vtj1_video.v.

module vtj1_vtt(
    // I/O device interface
    input clk, // system clock: rising edge active
    input rst, // system reset: active-high synchronous
    input [7:0] adr, // register address
    input [7:0] adr_d1, // 'adr' delayed by 1 clock cycle
    output reg [7:0] red, // data being read
    input [7:0] wrt, // data being written
    input wen, // enable write
    output irqa, // interrupt request ("alpha IRQ")
    output irqb,  // interrupt request ("beta IRQ")

    // Device specific interface
    input vsync, // the vertical sync signal
    input hsync, // the horizontal sync signal
    input rnz, // red != zero
    input gnz, // green != zero
    input bnz, // blue != zero
    input idis, // whether CPU has interrupts disabled
    input pe // pixel enable pulse train
);
    // settings
    reg [2:0]cfg_pary = 3'd0; // primary input channel selector
    reg [2:0]cfg_sary = 3'd0; // seconary input channel selector
    reg [3:0]cfg_logic = 4'd0; // logic function to combine them
    reg [31:0]cfg_thresh = 32'd0; // logic count threshold

    // extract & combine the input channels as selected
    reg pary, sary;
    always @*
        casex (cfg_pary)
        3'b000: pary = vsync;
        3'b001: pary = hsync;
        3'b010: pary = pe;
        3'b100: pary = rnz;
        3'b101: pary = gnz;
        3'b110: pary = bnz;
        default: pary = 1'b0;
        endcase
    always @*
        casex (cfg_sary)
        3'b000: sary = vsync;
        3'b001: sary = hsync;
        3'b010: sary = pe;
        3'b100: sary = rnz;
        3'b101: sary = gnz;
        3'b110: sary = bnz;
        default: sary = 1'b0;
        endcase
    reg comb;
    always @*
        case ({ sary, pary })
        2'b00: comb = cfg_logic[0];
        2'b01: comb = cfg_logic[1];
        2'b10: comb = cfg_logic[2];
        2'b11: comb = cfg_logic[3];
        endcase

    // count clock cycles in a row that 'comb == 1'
    // the top bit of the counter doesn't wrap around, it's stuck at 1
    // until reset.  That way we only increment the hit counter (later)
    // once per run of 1's.
    reg [32:0] cycles = 33'd0;
    wire [32:0] cycles_inc = cycles + 33'd1;
    always @(posedge clk)
        if (rst)
            // on system reset, restart counting cycles
            cycles <= 33'd0;
        else if (comb)
            // when comb == 1, count another cycle
            // the high bit of the cycle counter 'sticks' high so that
            // the counter won't wrap around
            cycles <= { cycles[32] | cycles_inc[32],
                        cycles_inc[31:0] };
        else
            // when comb == 0, restart counting cycles
            cycles <= 33'd0;

    // Count the number of times 'cycles' reaches the configured threshold.
    reg [31:0] hits = 32'd0;
    always @(posedge clk)
        if (rst)
            hits <= 32'd0; // on system reset, reset this
        else if (cycles == { 1'b0, cfg_thresh })
            hits <= hits + 32'd1; // count times it hit the threshold

    // Count clock cycles interrupts were enabled/disabled
    reg [31:0] ienc = 32'd0, idic = 32'd0;
    always @(posedge clk)
        ienc <= rst ? 32'd0 : (idis ? ienc : (ienc + 32'd1));
    always @(posedge clk)
        idic <= rst ? 32'd0 : (idis ? (idic + 32'd1) : idic);

    // Register interface for configuration and monitoring.
    reg [31:0] hits_l = 32'd0, // latched copies of counters
               ienc_l = 32'd0,
               idic_l = 32'd0;
    always @(posedge clk)
        if (rst) begin
            cfg_pary <= 3'd0;
            cfg_sary <= 3'd0;
            cfg_logic <= 4'd0;
            cfg_thresh <= 32'd0;
            red <= 8'd0;
            hits_l <= 32'd0;
            ienc_l <= 32'd0;
            idic_l <= 32'd0;
        end else begin
            case (adr[4:0])
            5'd0: red <= { 1'd0, cfg_sary, 1'd0, cfg_pary };
            5'd1: red <= { 4'd0, cfg_logic };
            5'd4: red <= hits_l[7:0];
            5'd5: red <= hits_l[15:8];
            5'd6: red <= hits_l[23:16];
            5'd7: red <= hits_l[31:24];
            5'd8: red <= ienc_l[7:0];
            5'd9: red <= ienc_l[15:8];
            5'd10: red <= ienc_l[23:16];
            5'd11: red <= ienc_l[31:24];
            5'd12: red <= idic_l[7:0];
            5'd13: red <= idic_l[15:8];
            5'd14: red <= idic_l[23:16];
            5'd15: red <= idic_l[31:24];
            5'd16: red <= cfg_thresh[7:0];
            5'd17: red <= cfg_thresh[15:8];
            5'd18: red <= cfg_thresh[23:16];
            5'd19: red <= cfg_thresh[31:24];
            default: red <= 8'd0;
            endcase
            if (wen) begin
                red <= wrt;
                case (adr[4:0])
                5'd0: begin
                    cfg_pary <= wrt[2:0];
                    cfg_sary <= wrt[6:4];
                end
                5'd1: cfg_logic <= wrt[3:0];
                5'd16: cfg_thresh[7:0] <= wrt;
                5'd17: cfg_thresh[15:8] <= wrt;
                5'd18: cfg_thresh[23:16] <= wrt;
                5'd19: cfg_thresh[31:24] <= wrt;
                default: begin
                    // In this case 'wrt' is ignored; the value doesn't
                    // matter, it's just a signal to make things happen.
                    hits_l <= hits;
                    ienc_l <= ienc;
                    idic_l <= idic;
                end
                endcase
            end
        end

    // And this doesn't do IRQs.
    assign irqa = 1'b0;
    assign irqb = 1'b0;
endmodule
