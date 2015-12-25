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

// vtj1_video.v
// Video generator device for my "VTJ-1" project.  Generates a 640x480 VGA
// picture containing an array of text characters.  Does horizontal timing &
// it's up to the software to handle the vertical timing.

// A word on stages and notation:  A lot of the signals here are marked with
// _s1, _s2, etc.  These are pipeline stages.  The signals with _s2 follow
// the ones with _s1, one pixel cycle ('pe' pulse) later.  Thus, a signal
// that derives combinationally (with 'always @*' and '=') from _sn signals,
// is an _sn signal too, while if it's synchronous (with 'always @(posedge clk)'
// and '<=') it's an _s(n+1) signal.

// The only exceptions are things that have state (where a signal is derived,
// not from a previous-stage signal, but from an *earlier* same-stage signal).

// Include vtj1-config.v: `defines that control the functionality of this
// system at build time.  Derived from vtj1-config.txt by vtj1-config-gen.tcl.
`include "vtj1-config.v"

module vtj1_video(
    // I/O device interface
    input clk, // system clock: rising edge active
    input rst, // system reset: active-high synchronous
    input [7:0] adr, // register address
    input [7:0] adr_d1, // 'adr' delayed by 1 clock cycle
    output reg [7:0] red, // data being read
                          // (short for "read", and not the color "red";
                          // it's my usual convention for bus signal names
                          // and it's a little unfortunate in this instance)
    input [7:0] wrt, // data being written
    input wen, // enable write
    output irqa, // interrupt request ("alpha IRQ")
    output irqb,  // interrupt request ("beta IRQ")

    // Device specific interface (external)
    output o_vsync, // vertical sync signal
    output o_hsync, // horizontal sync signal
    output [1:0] o_video_r, // output signal, red
    output [1:0] o_video_g, // output signal, green 
    output [1:0] o_video_b, // output signal, blue

    // Device specific interface (internal)
    output [11:0]text_adr, // address for accessing text memory
    input [15:0]text_red, // result of reading text memory
    output [(CCWID+3):0]font_adr, // address for accessing font memory
    input [7:0]font_red, // result of reading font memory
    input pe // should pulse high once for each pixel
);
    // CCWID - Width of character codes in bits.  Changing this, changes the
    // font format, thus requiring changes when running vtj1fonter.tcl to
    // translate fonts.  Suggested values 7 or 8.
    parameter CCWID = 8;

    // This module presents a few registers on the CPU bus; accesses
    // font and text memory on separate buses; and generates video signals.
    // The rest of it takes the place of a pipeline without feedback.

    // First pipeline stage: horizontal timing.
    // Output signals:
    //      xtim_s1 - X coordinate (which is also a state variable)
    //      xviz_s1 - indicates xtim_s1 is in visible area
    //      xsyp_s1 - indicates xtim_s1 is in the horizontal sync pulse
    //      x1st_s1 - indicates first pixel of new line
    reg [9:0] xtim_s1 = 10'd0;
    reg xviz_s1 = 1'b0, xsyp_s1 = 1'b0, x1st_s1 = 1'b0;
    wire [9:0] xtim_inc = xtim_s1 + 1;
    always @(posedge clk)
        if (rst) begin
            xtim_s1 <= 10'd0;
            xviz_s1 <= 1'b0;
            xsyp_s1 <= 1'b0;
            x1st_s1 <= 1'b0;
        end else if (pe) begin
            xtim_s1 <= xtim_inc;
            xviz_s1 <= 1'b0;
            xsyp_s1 <= 1'b0;
            x1st_s1 <= 1'b0;

            casex (xtim_inc[9:4])
            6'b0?_????: xviz_s1 <= 1'b1; // 0 - 511 -- visible
            6'b10_0???: xviz_s1 <= 1'b1; // 512 - 639 -- visible
            // 6'b10_1000:               // 640 - 655 -- front porch
            6'b10_1001: xsyp_s1 <= 1'b1; // 656 - 671 -- sync pulse
            6'b10_101?: xsyp_s1 <= 1'b1; // 672 - 703 -- sync pulse
            6'b10_110?: xsyp_s1 <= 1'b1; // 704 - 735 -- sync pulse
            6'b10_1110: xsyp_s1 <= 1'b1; // 736 - 751 -- sync pulse
            // 6'b10_1111:               // 752 - 767 -- back porch
            // 6'b11_000?:               // 768 - 799 -- back porch
            6'b11_001?: begin            // 800 -- next line
                             xtim_s1 <= 10'd0;
                             xviz_s1 <= 1'b1;
                             x1st_s1 <= 1'b1;
            end
            // default: begin end;       // shouldn't happen
            endcase
    end

    reg x1st_s2 = 1'b0, xviz_s2 = 1'b0, xsyp_s2 = 1'b0;
    always @(posedge clk)
        if (rst) begin
            x1st_s2 <= 1'b0;
            xviz_s2 <= 1'b0;
            xsyp_s2 <= 1'b0;
        end else if (pe) begin
            x1st_s2 <= x1st_s1;
            xviz_s2 <= xviz_s1;
            xsyp_s2 <= xsyp_s1;
        end

    // relay/delay signals from stage 1 to stage 2
    // (pipeline stage 2)
    // Control register interface to CPU.
    // The following inputs and outputs refer to the pipeline.
    // Products:
    //      rowctl_s2 -- the row control fields for the current row;
    //                   also a state variable
    //          rowctl_s2[3:0] - what pixel row to use within the font
    //          rowctl_s2[4] - double width characters
    //          rowctl_s2[5] - this line is the underline line
    //          rowctl_s2[6] - enable video output
    //          rowctl_s2[7] - vertical sync value (0 = pulse, 1 = normal)
    //          rowctl_s2[15:8] - starting text address
    //      supr_s2 -- suppress video output
    //      suprsy_s2 -- suppress sync output
    //      inverse_scrn -- screenwise inverse video (note, this signal
    //          isn't kept in sync with pipeline stages, because its timing
    //          isn't important)
    //      inverse_scrn2 -- same purpose as inverse_scrn; the reason there
    //          are two such flags is to make visible bell easier to code
    // Some important internal signals:
    //      rowctl_nxt -- value set by the CPU that will become the
    //          next rowctl_s2
    //      insan -- sanity checks failed
    //      insan_code -- code for why they failed

    reg [15:0] rowctl_s2 = 16'd0; // currently operating row control
    reg [15:0] rowctl_nxt = 16'd0; // row control for next pixel row
    reg rowup_i = 1'b0, rowup_o = 1'b0; // differ if row has been updated
    reg [3:0] insan_code = 4'b0;
    wire insan = |insan_code;
    reg insan_clr = 1'b0; // pulse to clear insan_code
    reg inverse_scrn = 1'b0; // screenwise inverse video
    reg inverse_scrn2 = 1'b0; // screenwise inverse video, again

    `define INSAN_CODE_UPDATE_FAILURE 4'h5
    `define INSAN_CODE_VIDEO_DURING_SYNC 4'ha
    `define INSAN_CODE_CLEAR 4'h7
    `define ALTER_FLAGS2_CMD 4'he
    `define ALTER_FLAGS3_CMD 4'hf

    wire row_update_needed = (rowup_i == rowup_o);

    // taking new scan line (row) information
    always @(posedge clk)
        if (rst) begin
            rowctl_s2 <= 16'd0;
            rowup_o <= rowup_i;
        end else if (pe && x1st_s1) begin
            rowctl_s2 <= rowctl_nxt;
            rowup_o <= rowup_i;
        end

    // sanity check failures
    always @(posedge clk)
        if (rst)
            insan_code <= 4'd0;
        else if (insan_clr)
            insan_code <= 4'd0;
        else if (pe && x1st_s1 && !insan) begin
            if (row_update_needed)
                insan_code <= `INSAN_CODE_UPDATE_FAILURE;
            else if (rowctl_nxt[7:6] == 2'b01)
                insan_code <= `INSAN_CODE_VIDEO_DURING_SYNC;
        end

    // register access
    always @(posedge clk)
        if (rst) begin
            rowctl_nxt <= 16'd0;
            insan_clr <= 1'b0;
            inverse_scrn <= 1'b0;
            inverse_scrn2 <= 1'b0;
        end else begin
            insan_clr <= 1'b0;
            case (adr[1:0])
            2'd0: red <= rowctl_nxt[7:0];
            2'd1: red <= rowctl_nxt[15:8];
            2'd2: red <= { 2'd0, inverse_scrn2, inverse_scrn, insan_code };
            default: red <= 8'd0;
            endcase
            if (wen) begin
                red <= wrt;
                case (adr[1:0])
                2'd0: begin
                    rowctl_nxt[7:0] <= wrt;
                    rowup_i <= !rowup_o; // indicate a row update has been done
                end
                2'd1: rowctl_nxt[15:8] <= wrt;
                2'd2: begin
                    if (wrt[3:0] == `INSAN_CODE_CLEAR)
                        // Clear insan_code
                        insan_clr <= 1'b1;
                    if (wrt[3:0] == `ALTER_FLAGS2_CMD)
                        // Alter flag a value in high bits of this byte
                        inverse_scrn <= wrt[4];
                    if (wrt[3:0] == `ALTER_FLAGS3_CMD)
                        // Alter flag a value in high bits of this byte
                        inverse_scrn2 <= wrt[5];
                end
                endcase
            end
        end

    // the effect of some of those, to suppress output
    assign supr_s2 = insan || !xviz_s2 || !rowctl_s2[6];
    assign suprsy_s2 = insan;

    // interrupt requests (IRQs) to the processor
    assign irqa = row_update_needed;
    assign irqb = 1'b0;

    // relay/delay signals from stage 2 to stage 3
    reg [15:0] rowctl_s3;
    reg x1st_s3 = 1'b0, xsyp_s3 = 1'b0, supr_s3 = 1'b1, suprsy_s3 = 1'b1;
    always @(posedge clk)
        if (rst) begin
            rowctl_s3 <= 16'd0;
            x1st_s3 <= 1'b0;
            xsyp_s3 <= 1'b0;
            supr_s3 <= 1'b1;
            suprsy_s3 <= 1'b1;
        end else if (pe) begin
            rowctl_s3 <= rowctl_s2;
            x1st_s3 <= x1st_s2;
            xsyp_s3 <= xsyp_s2;
            supr_s3 <= supr_s2;
            suprsy_s3 <= suprsy_s2;
        end

    // (pipeline stage 3)
    // Single- and double- width character timings; and character cell
    // addressing.
    // Products:
    //      fontpx_s3 - pulse when a new pixel should be displayed from
    //                  the font
    //      fontch_s3 - pulse when a new character cell should be displayed
    //      cellad_s3 - aka text_adr; addresses the character cell in
    //                  text RAM
    // Internal signals:
    //      fontctr - counts pixels horizontally within a character
    //      cellad_prv - previous 'cellad_s3' value
    reg [4:0] fontctr = 5'd31;
    wire [4:0] fontctr_inc = fontctr + 5'd1;
    always @(posedge clk)
        if (rst)
            fontctr <= 5'd31;
        else if (pe) begin
            if (x1st_s2)
                fontctr <= 5'd31;
            else
                fontctr <= fontctr_inc;
        end
    wire [4:0] fontctr_chg = fontctr ^ fontctr_inc;
    wire fontpx_s3 = rowctl_s3[4] ? fontctr_chg[1] : fontctr_chg[0];
    wire fontch_s3 = rowctl_s3[4] ? fontctr_chg[4] : fontctr_chg[3];

    reg [11:0] cellad_prv = 12'd0;
    reg [11:0] cellad_s3;
    always @*
        if (x1st_s3)
            // start of new row: where it's configured to go
            cellad_s3 = { rowctl_s3[15:8], 4'd0 };
        else if (fontch_s3)
            // next character cell after the last one
            cellad_s3 = cellad_prv + 12'd1;
        else
            // same one as before
            cellad_s3 = cellad_prv;

    always @(posedge clk)
        if (rst)
            cellad_prv <= 12'd0;
        else if (pe)
            cellad_prv <= cellad_s3;

    assign text_adr = cellad_s3;

    // relay/delay signals from stage 3 to stage 4
    reg [15:0] rowctl_s4 = 16'd0;
    reg fontpx_s4 = 1'd0, fontch_s4 = 1'd0, xsyp_s4 = 1'b0;
    reg supr_s4 = 1'b1, suprsy_s4 = 1'b1;
    always @(posedge clk)
        if (rst) begin
            rowctl_s4 <= 16'd0;
            fontpx_s4 <= 1'd0;
            fontch_s4 <= 1'd0;
            xsyp_s4 <= 1'd0;
            supr_s4 <= 1'd1;
            suprsy_s4 <= 1'd1;
        end else if (pe) begin
            rowctl_s4 <= rowctl_s3;
            fontpx_s4 <= fontpx_s3;
            fontch_s4 <= fontch_s3;
            xsyp_s4 <= xsyp_s3;
            supr_s4 <= supr_s3;
            suprsy_s4 <= suprsy_s3;
        end

    // (pipeline stage 4)
    // Text memory access has completed; address font memory.
    // Products:
    //      cell_s4 - character cell contents retrieved from text RAM
    //          cell_s4[7:0] - character code
    //          cell_s4[10:8] - foreground color; [0] is red
    //          cell_s4[11] - brighten foreground color
    //          cell_s4[14:12] - background color
    //          cell_s4[15] - underline
    //      fontad_s4 - aka font_adr; byte address in font RAM as derived
    //          from cell_s4 and other things

`ifndef VTJ1_VIDEO_25MHZ
    reg [15:0] cell_s4 = 16'd0;
    always @(posedge clk)
        if (rst)
            cell_s4 <= 16'd0;
        else if (pe)
            cell_s4 <= text_red;
`else
    // the external text memory already puts a one clock cycle delay in...
    wire [15:0] cell_s4 = text_red;
`endif
    wire [(CCWID+3):0] fontad_s4 = {
        rowctl_s4[3:0], // pixel row within the font's characters
        cell_s4[(CCWID-1):0] // character code retrieved from text RAM
    };
    assign font_adr = fontad_s4;

    // relay/delay signals from stage 4 to stage 5
    reg [15:0] rowctl_s5 = 16'd0, cell_s5 = 16'd0;
    reg fontpx_s5 = 1'd0, fontch_s5 = 1'd0;
    reg xsyp_s5 = 1'b0, supr_s5 = 1'b1, suprsy_s5 = 1'b1;
    always @(posedge clk)
        if (rst) begin
            rowctl_s5 <= 16'd0;
            cell_s5 <= 16'd0;
            fontpx_s5 <= 1'd0;
            fontch_s5 <= 1'd0;
            xsyp_s5 <= 1'd0;
            supr_s5 <= 1'd1;
            suprsy_s5 <= 1'd1;
        end else if (pe) begin
            rowctl_s5 <= rowctl_s4;
            cell_s5 <= cell_s4;
            fontpx_s5 <= fontpx_s4;
            fontch_s5 <= fontch_s4;
            xsyp_s5 <= xsyp_s4;
            supr_s5 <= supr_s4;
            suprsy_s5 <= suprsy_s4;
        end

    // (pipeline stage 5)
    // Font memory access has completed; that produces 8 bits that
    // each describe a pixel (pulse of 'pe && fontpx_s4') and will be
    // shifted left.
    // Products:
    //      font_dat_s5 - 8 bits read from font RAM
    //      font_sr_s5 - shift register shifting bits out of it;
    //          its top bit ([7]) is the current output pixel
    // Internal signals:
    //      font_sr_prv - previous value of font_sr_s5
`ifndef VTJ1_VIDEO_25MHZ
    reg [7:0] font_dat_s5 = 8'd0;
    always @(posedge clk)
        if (rst)
            font_dat_s5 <= 8'd0;
        else if (pe)
            font_dat_s5 <= font_red;
`else
    // the external text memory already puts a one clock cycle delay in...
    wire [15:0] font_dat_s5 = font_red;
`endif
    reg [7:0] font_sr_s5;
    reg [7:0] font_sr_prv = 8'd0;

    always @(posedge clk)
        if (rst)
            font_sr_prv <= 8'd0;
        else if (pe)
            font_sr_prv <= font_sr_s5;

    always @*
        if (fontch_s5)
            // new character cell; got its data from font RAM
            font_sr_s5 = font_dat_s5;
        else if (fontpx_s5)
            // new output pixel; shift the last one out
            font_sr_s5 = { font_sr_prv[6:0], 1'b0 };
        else
            font_sr_s5 = font_sr_prv;

    // With pipeline stage 5, we finally have determined pixel
    // foreground/background and character color.
    //
    // Internal signals:
    //      pxfg - 1 for a foreground pixel, 0 for a background pixel
    //      pxcolor - 4 bit color for pixel

    wire pxfg = (cell_s5[15] && rowctl_s5[5]) || // underline
                font_sr_s5[7]; // from font

    wire [3:0] pxcolor = (inverse_scrn ^ inverse_scrn2 ^ pxfg) ?
                         cell_s5[11:8] :
                         { 1'b0, cell_s5[14:12] };

    // pipeline stage 6 - the actual outputs
    reg o_vsync_s6 = 1'b1, o_hsync_s6 = 1'b1;
    reg [1:0] o_video_r_s6 = 2'd0,
              o_video_g_s6 = 2'd0,
              o_video_b_s6 = 2'd0;

    always @(posedge clk)
        if (rst) begin
            o_vsync_s6 <= 1'b1;
            o_hsync_s6 <= 1'b1;
            o_video_r_s6 <= 2'd0;
            o_video_g_s6 <= 2'd0;
            o_video_b_s6 <= 2'd0;
        end else if (pe) begin
            o_vsync_s6 <= rowctl_s5[7] || suprsy_s5;
            o_hsync_s6 <= (!xsyp_s5) || suprsy_s5;
            o_video_r_s6 <= supr_s5 ? 2'b0 : { pxcolor[0], pxcolor[3] };
            o_video_g_s6 <= supr_s5 ? 2'b0 : { pxcolor[1], pxcolor[3] };
            o_video_b_s6 <= supr_s5 ? 2'b0 : { pxcolor[2], pxcolor[3] };
        end

    assign o_vsync = o_vsync_s6;
    assign o_hsync = o_hsync_s6;
    assign o_video_r = o_video_r_s6;
    assign o_video_g = o_video_g_s6;
    assign o_video_b = o_video_b_s6;
endmodule
