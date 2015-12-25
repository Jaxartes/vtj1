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

// sh_vtj1_video.v
// "Simulation harness" for testing vtj1_video.v within the Icarus Verilog
// simulator.  Not synthesizable.

module sh_vtj1_video(
    // top level module: no signals
);

    // clock generator; instead of using actual time let's just use 100 time
    // units per clock cycle, 300 per pixel
    reg clk = 0;
    initial begin
        forever begin
            #50;
            clk <= ~clk;
        end
    end

    // reset generator; make reset high for 1000 time units (ten clock
    // cycles)
    reg rst = 1;
    initial begin
        #1000;
        rst <= 0;
    end

    // pixel clock enable signal, 1/3 the clock rate
    reg [2:0]pixel_oh = 3'd1;
    always @(posedge clk)
        if (rst)
            pixel_oh <= 3'd1;
        else
            pixel_oh <= { pixel_oh[1:0], pixel_oh[2] };
    wire pixel_enable = pixel_oh[2];

    // font "memory", provides a single character, a sort of square
    // bulls-eye
    wire [11:0] font_vadr;
    reg [7:0] font_vred = 8'd0;
    always @(posedge clk)
        if (rst)
            font_vred <= 8'h00;
        else case(font_vadr[11:8])
        4'd0: font_vred <= 8'hff;
        4'd1: font_vred <= 8'h81;
        4'd2: font_vred <= 8'h81;
        4'd3: font_vred <= 8'h99;
        4'd4: font_vred <= 8'h99;
        4'd5: font_vred <= 8'h81;
        4'd6: font_vred <= 8'h81;
        4'd7: font_vred <= 8'hff;
        default: font_vred <= 8'h00;
        endcase

    // text "memory", provides a cycle among foreground colors 1-15
    wire [11:0] text_vadr;
    reg [15:0] text_vred = 16'd0;
    always @(posedge clk)
        if (rst)
            text_vred <= 16'd0;
        else begin
            text_vred[11:8] <= 1 + (text_vadr % 15);
        end

    // "cpu" bus interface: the only thing that happens here is
    // that some sample values are written to the three device registers,
    // cyclically:
    //      reg 0 (scan line control byte 0): 8'hc1 (for scan line 1)
    //      reg 1 (scan line control byte 1): 8'h00 (for text row 0)
    //      reg 2 (special command and status): 8'h07 (clear errors)
    reg [7:0] bus_adr, bus_wrt;
    wire bus_wen = 1'b1;
    initial forever begin
        @(posedge clk)
        @(negedge clk)
        bus_adr <= 8'h00; bus_wrt <= 8'hc1;
        @(posedge clk)
        @(negedge clk)
        bus_adr <= 8'h01; bus_wrt <= 8'h00;
        @(posedge clk)
        @(negedge clk)
        bus_adr <= 8'h02; bus_wrt <= 8'h07;
    end

    // the video core
    wire o_vsync, o_hsync;
    wire [1:0] o_video_r, o_video_g, o_video_b;
    vtj1_video v(
        .clk(clk), .rst(rst), .adr(bus_adr),
        .wrt(bus_wrt), .wen(bus_wen),
        .o_video_r(o_video_r), .o_video_g(o_video_g), .o_video_b(o_video_b),
        .text_adr(text_vadr), .font_adr(font_vadr),
        .text_red(text_vred), .font_red(font_vred),
`ifdef VTJ1_VIDEO_25MHZ
        .pe(1)
`else
        .pe(pixel_enable)
`endif
    );

    // monitoring for debugging purposes
    simmon #(1, "rst") sm0(clk, rst);
    simmon #(6, "color") sm1(clk, { o_video_b, o_video_g, o_video_r });
    simmon #(1, "pe") sm2(clk, pixel_enable);
    simmon #(12, "text_vadr") sm3(clk, text_vadr);
    simmon #(12, "font_vadr") sm4(clk, font_vadr);
    simmon #(16, "text_vred") sm5(clk, text_vred);
    simmon #(8, "font_vred") sm6(clk, font_vred);

    simmon #(16, "cell_s4") sm13(clk, v.cell_s4);
    simmon #(1, "fontch_s4") sm11(clk, v.fontch_s4);
    simmon #(1, "fontpx_s4") sm12(clk, v.fontpx_s4);

    simmon #(1, "pxfg") sm9(clk, v.pxfg);
    simmon #(4, "pxcolor") sm10(clk, v.pxcolor);
endmodule

module simmon(input CLK, input [(WIDTH-1):0] sig);
    parameter WIDTH = 1;
    parameter ID = "LED1";
    reg [(WIDTH-1):0] saved;
    reg saved_at_all = 0;
    always @(posedge CLK) begin
        if (!saved_at_all)
            $display("%t: %s: ->%x", $time, ID, sig);
        else if (saved !== sig)
            $display("%t: %s: %x->%x", $time, ID, saved, sig);
        saved <= sig;
        saved_at_all <= 1;
    end
endmodule
