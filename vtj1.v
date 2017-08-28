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

// vtj1.v
//
// System superstructure for "VTJ-1" project.
// Combines Arlet Ottens' implementation of the 6502
// processor with RAM and devices.  See vtj1-overview.txt and vtj1-design.txt
// for more information.

// Include vtj1-config.v: `defines that control the functionality of this
// system at build time.  Derived from vtj1-config.txt by vtj1-config-gen.tcl.
`include "vtj1-config.v"

module vtj1(
    input board_clk, // input clock from the circuit board; on Papilio Pro
                     // boards it's 32MHz
    // buttons and lights
    input i_reset,
    output [3:0]leds,
    output led1,
    // video output
    output o_vsync,
    output o_hsync,
`ifdef VTJ1_COLOR_444
    output [3:0]o_video_r,
    output [3:0]o_video_g,
    output [3:0]o_video_b,
`endif
`ifdef VTJ1_COLOR_332
    output [3:1]o_video_r,
    output [3:1]o_video_g,
    output [3:2]o_video_b,
`endif
    // PS/2 port A
    inout ps2a_clk,
    inout ps2a_dat,
    // audio output
    output o_audio_l,
    output o_audio_r,
    // serial port - through the FTDI chip or wherever you want it to go
    input rx_serial,
    output tx_serial
);
    // CCWID - character code width in bits
    // FONTSZ - font size in bytes
    // both are derived from FONT_256_CHARS and shouldn't be changed on
    // their own.
`ifdef FONT_256_CHARS
    parameter CCWID = 8;
    parameter FONTSZ = 4096;
`else
    parameter CCWID = 7;
    parameter FONTSZ = 2048;
`endif

    // System clock derived from the clock input board_clk
    wire clk, clklck;
    vtj1_clock vtj1_clock(
        .iclk(board_clk), .oclk(clk), .locked(clklck)
    );

    // And pixel clock enable signal.  It consists of a single clock
    // cycle pulse at ~25MHz, in 'pixel_enable'.  PIXEL_ENABLE_RATE
    // should be the clock rate divided by 25MHz.
    // If it's 1, then PIXEL_ENABLE_RATE_1 should also be `defined.
    // The log to make that happen lives in vtj1-config-gen.tcl

`ifdef PIXEL_ENABLE_RATE_1
    wire pixel_enable = 1'b1;
`else
    reg [(`PIXEL_ENABLE_RATE-1):0]pixel_oh = `PIXEL_ENABLE_RATE'd1;
    always @(posedge clk)
        if (rst)
            pixel_oh <= `PIXEL_ENABLE_RATE'd1;
        else
            pixel_oh <= { pixel_oh[(`PIXEL_ENABLE_RATE-2):0],
                          pixel_oh[`PIXEL_ENABLE_RATE-1] };
    wire pixel_enable = pixel_oh[0];
`endif

    // Reset logic.  Driven by the reset button and the clock's "LOCKED"
    // signal.

    reg rst2 = 1, rst3 = 1, rst4 = 1;
    reg [3:0] rstctr = 0;
    always @(posedge clk) begin
        // synchronize the incoming reset button signal
        { rst3, rst4 } <= { rst4, i_reset };
        // count how long it's been since the button went up
        if (rst3 || ~clklck) begin
            rstctr <= 0;
            rst2 <= 1;
        end else if (rstctr != 4'b1111) begin
            rstctr <= rstctr + 4'd1;
            rst2 <= 1;
        end else
            rst2 <= 0;
    end
    wire rst = rst2 || ~clklck;

    // Major signals.
    wire [15:0] bus_adr;
    wire [7:0] bus_wrt;
    reg [7:0] bus_red;
    wire bus_wen;
    wire cpu_irq;
    reg ram_wen, text_wen, font_wen, io_wen, rom_wen;
    wire [11:0] text_vadr, font_vadr;
    reg [15:0] text_vred = 16'd0;
    reg [7:0] font_vred = 8'd0;
    wire idis;

    // Here's the 6502 CPU.
    cpu cpu6502(
        .clk(clk), // clock: positive edge active
        .reset(rst), // reset: synchronous active high
        .AB(bus_adr),
        .DI(bus_red),
        .DO(bus_wrt),
        .WE(bus_wen),
        .IRQ(cpu_irq),
        .NMI(1'b0),
        .RDY(1'b1),
        .idis(idis)
    );

    reg [15:0] bus_adr_d1;
    always @(posedge clk) bus_adr_d1 <= bus_adr;

    // Master baud rate generator, for use by serial devices.  Produces
    // chains of single-clock-cycle pulses at 7,372,800 per second, and
    // half that, quarter that, down to 1/2^13 that.  Serial ports and
    // timers will select from those the one configured by their internal
    // registers, and optionally divide it by 3.

    // `MASTERBAUD_RATE is a constant defined in vtj1-config.v that
    // controls the generation of these pulses in proportion to the
    // current clock rate.  It's 483183.8208 / system_clock_MHz.
    // Its value is computed in vtj1-config-gen.tcl.
    reg [29:0] masterbaud_ctr = 30'd0;
    wire [29:0] masterbaud_inc = masterbaud_ctr + `MASTERBAUD_RATE;
    always @(posedge clk) masterbaud_ctr <= masterbaud_inc;
    wire [13:0] masterbaud =
        masterbaud_ctr[29:16] ^
        masterbaud_inc[29:16];
    wire led_dimming_ctl = masterbaud_ctr[29];

    // And for PS/2 ports we want a train of single cycle pulses every 6us
    // to control the timing of that port.  That's 11 pulses of
    // masterbaud[2].
    reg [3:0] sixus_ctr = 4'd0;
    wire sixus = (sixus_ctr == 4'd10);
    always @(posedge clk)
        if (masterbaud[2]) begin
            if (sixus)
                sixus_ctr <= 1'd0;
            else
                sixus_ctr <= sixus_ctr + 4'd1;
        end

    // General purpose RAM, $0000
`ifdef BIG_RAM
    reg [7:0] ram[0:2047];
    wire [10:0] ram_adr = bus_adr[10:0];
`else
    reg [7:0] ram[0:1023];
    wire [9:0] ram_adr = bus_adr[9:0];
`endif
    reg [7:0] ram_red = 8'd0;
    always @(posedge clk) begin
        ram_red <= ram[ram_adr];
        if (ram_wen) begin
            ram[ram_adr] <= bus_wrt;
            ram_red <= bus_wrt;
        end
    end

    // Text RAM, $8000.  Dual ported.
    reg [7:0] text0[0:4095], text1[0:4095];
    reg [7:0] text0_red = 8'd0, text1_red = 8'd0;
    always @(posedge clk) begin
        text0_red <= text0[bus_adr[12:1]];
        if (text_wen && !bus_adr[0]) begin
            text0_red <= bus_wrt;
            text0[bus_adr[12:1]] <= bus_wrt;
        end
    end
    always @(posedge clk) begin
        text1_red <= text1[bus_adr[12:1]];
        if (text_wen && bus_adr[0]) begin
            text1_red <= bus_wrt;
            text1[bus_adr[12:1]] <= bus_wrt;
        end
    end
    wire [7:0] text_red = bus_adr_d1[0] ? text1_red : text0_red;
    always @(posedge clk)
        if (rst)
            text_vred <= 16'd0;
        else
            text_vred <= { text1[text_vadr], text0[text_vadr] };

    // Font RAM, $a000.  Dual ported.
    reg [7:0] font[0:(FONTSZ-1)];
    reg [7:0] font_red = 8'd0;
    always @(posedge clk) begin
        font_red <= font[bus_adr[11:0]];
`ifdef WRITEABLE_FONT
        if (font_wen) begin
            font_red <= bus_wrt;
            font[bus_adr[(CCWID+3):0]] <= bus_wrt;
        end
`endif // WRITEABLE_FONT
    end
    initial $readmemh("font.mem", font, 0, FONTSZ - 1);
    always @(posedge clk)
        if (rst)
            font_vred <= 8'd0;
        else
            font_vred <= font[font_vadr];

    // I/O space, $b000-$bfff, with 16 devices.
    wire [15:0] alpha_irqs;
    wire [15:0] beta_irqs;
    reg [7:0] io_red;
    wire [7:0] io_red0, io_red1, io_red2, io_red3;
    wire [7:0] io_red4, io_red5, io_red6, io_red7;
    wire [7:0] io_red8, io_red9, io_red10, io_red11;
    wire [7:0] io_red12, io_red13, io_red14, io_red15;

    // I/O slot 0 - system control (mainly IRQ management)
    vtj1_sysc io0(
        .clk(clk), .rst(rst), .adr(bus_adr[7:0]), .adr_d1(bus_adr_d1[7:0]),
        .red(io_red0), .wrt(bus_wrt), .wen(io_wen && bus_adr[11:8] == 4'd0),
        .irqa(alpha_irqs[0]), .irqb(beta_irqs[0]),
        .alpha_irqs(alpha_irqs), .beta_irqs(beta_irqs),
        .irq(cpu_irq), .masterbaud(masterbaud)
    );

    // I/O slot 1 - video output
    vtj1_video #(
`ifdef FONT_256_CHARS
        .CCWID(8)
`else
        .CCWID(7)
`endif
    ) io1(
        .clk(clk), .rst(rst), .adr(bus_adr[7:0]), .adr_d1(bus_adr_d1[7:0]),
        .red(io_red1), .wrt(bus_wrt), .wen(io_wen && bus_adr[11:8] == 4'd1),
        .irqa(alpha_irqs[1]), .irqb(beta_irqs[1]),
        .o_vsync(o_vsync), .o_hsync(o_hsync),
        .o_video_r(o_video_r[3:2]),
        .o_video_g(o_video_g[3:2]),
        .o_video_b(o_video_b[3:2]),
        .text_adr(text_vadr), .font_adr(font_vadr),
        .text_red(text_vred), .font_red(font_vred), .pe(pixel_enable)
    );
`ifdef VTJ1_COLOR_444
    assign o_video_r[1:0] = o_video_r[3:2];
    assign o_video_g[1:0] = o_video_g[3:2];
    assign o_video_b[1:0] = o_video_b[3:2];
`endif
`ifdef VTJ1_COLOR_332
    assign o_video_r[1] = o_video_r[3];
    assign o_video_g[1] = o_video_g[3];
`endif

    // I/O slot 2 - buttons and lights
    wire write_rom, beep;
    vtj1_gpio #(
        .NBTN(4'd0), // doesn't use the Arcade MegaWing directional buttons
        .NLED(4'd5)
    ) io2(
        .clk(clk), .rst(rst), .adr(bus_adr[7:0]), .adr_d1(bus_adr_d1[7:0]),
        .red(io_red2), .wrt(bus_wrt), .wen(io_wen && bus_adr[11:8] == 4'd2),
        .irqa(alpha_irqs[2]), .irqb(beta_irqs[2]),
        .leds({ led1, leds }),
        .write_rom(write_rom), .beep(beep),
        .dimctl(led_dimming_ctl) // don't light the LEDs all the time,
                                 // just 50% duty cycle at 900Hz
    );

    // I/O slot 3 - empty
    dummyio io3(
        .clk(clk), .rst(rst), .adr(bus_adr[7:0]), .adr_d1(bus_adr_d1[7:0]),
        .red(io_red3), .wrt(bus_wrt), .wen(io_wen && bus_adr[11:8] == 4'd3),
        .irqa(alpha_irqs[3]), .irqb(beta_irqs[3])
    );

    // I/O slot 4 - empty
    dummyio io4(
        .clk(clk), .rst(rst), .adr(bus_adr[7:0]), .adr_d1(bus_adr_d1[7:0]),
        .red(io_red4), .wrt(bus_wrt), .wen(io_wen && bus_adr[11:8] == 4'd4),
        .irqa(alpha_irqs[4]), .irqb(beta_irqs[4])
    );

    // I/O slot 5 - empty
    dummyio io5(
        .clk(clk), .rst(rst), .adr(bus_adr[7:0]), .adr_d1(bus_adr_d1[7:0]),
        .red(io_red5), .wrt(bus_wrt), .wen(io_wen && bus_adr[11:8] == 4'd5),
        .irqa(alpha_irqs[5]), .irqb(beta_irqs[5])
    );

    // I/O slot 6 - empty
    dummyio io6(
        .clk(clk), .rst(rst), .adr(bus_adr[7:0]), .adr_d1(bus_adr_d1[7:0]),
        .red(io_red6), .wrt(bus_wrt), .wen(io_wen && bus_adr[11:8] == 4'd6),
        .irqa(alpha_irqs[6]), .irqb(beta_irqs[6])
    );

    // I/O slot 7 - video timing tester
`ifdef VIDEO_TIMING_TESTER
    vtj1_vtt io7(
        .clk(clk), .rst(rst), .adr(bus_adr[7:0]), .adr_d1(bus_adr_d1[7:0]),
        .red(io_red7), .wrt(bus_wrt), .wen(io_wen && bus_adr[11:8] == 4'd7),
        .irqa(alpha_irqs[7]), .irqb(beta_irqs[7]),
        .vsync(o_vsync), .hsync(o_hsync),
        .rnz(o_video_r != 4'd0),
        .gnz(o_video_g != 4'd0),
        .bnz(o_video_b != 4'd0),
        .idis(idis), .pe(pixel_enable)
    );
`else
    dummyio io7(
        .clk(clk), .rst(rst), .adr(bus_adr[7:0]), .adr_d1(bus_adr_d1[7:0]),
        .red(io_red7), .wrt(bus_wrt), .wen(io_wen && bus_adr[11:8] == 4'd7),
        .irqa(alpha_irqs[7]), .irqb(beta_irqs[7])
    );
`endif

    // I/O slot 8 - serial port: FTDI
    vtj1_uart io8(
        .clk(clk), .rst(rst), .adr(bus_adr[7:0]), .adr_d1(bus_adr_d1[7:0]),
        .red(io_red8), .wrt(bus_wrt), .wen(io_wen && bus_adr[11:8] == 4'd8),
        .irqa(alpha_irqs[8]), .irqb(beta_irqs[8]),
        .rx_raw(rx_serial), .tx(tx_serial), .masterbaud(masterbaud)
    );

    // I/O slot 9 - empty; for second serial port if there is one
    dummyio io9(
        .clk(clk), .rst(rst), .adr(bus_adr[7:0]), .adr_d1(bus_adr_d1[7:0]),
        .red(io_red9), .wrt(bus_wrt), .wen(io_wen && bus_adr[11:8] == 4'd9),
        .irqa(alpha_irqs[9]), .irqb(beta_irqs[9])
    );

    // I/O slot 10 - empty
    dummyio io10(
        .clk(clk), .rst(rst), .adr(bus_adr[7:0]), .adr_d1(bus_adr_d1[7:0]),
        .red(io_red10), .wrt(bus_wrt), .wen(io_wen && bus_adr[11:8] == 4'd10),
        .irqa(alpha_irqs[10]), .irqb(beta_irqs[10])
    );

    // I/O slot 11 - empty
    dummyio io11(
        .clk(clk), .rst(rst), .adr(bus_adr[7:0]), .adr_d1(bus_adr_d1[7:0]),
        .red(io_red11), .wrt(bus_wrt), .wen(io_wen && bus_adr[11:8] == 4'd11),
        .irqa(alpha_irqs[11]), .irqb(beta_irqs[11])
    );

    // I/O slot 12 - PS/2 port A
    vtj1_ps2 io12(
        .clk(clk), .rst(rst), .adr(bus_adr[7:0]), .adr_d1(bus_adr_d1[7:0]),
        .red(io_red12), .wrt(bus_wrt), .wen(io_wen && bus_adr[11:8] == 4'd12),
        .irqa(alpha_irqs[12]), .irqb(beta_irqs[12]),
        .ps2clk(ps2a_clk), .ps2dat(ps2a_dat), .sixus(sixus)
    );

    // I/O slot 13 - empty; for second PS/2 port if there is one
    dummyio io13(
        .clk(clk), .rst(rst), .adr(bus_adr[7:0]), .adr_d1(bus_adr_d1[7:0]),
        .red(io_red13), .wrt(bus_wrt), .wen(io_wen && bus_adr[11:8] == 4'd13),
        .irqa(alpha_irqs[13]), .irqb(beta_irqs[13])
    );

    // I/O slot 14 - empty
    dummyio io14(
        .clk(clk), .rst(rst), .adr(bus_adr[7:0]), .adr_d1(bus_adr_d1[7:0]),
        .red(io_red14), .wrt(bus_wrt), .wen(io_wen && bus_adr[11:8] == 4'd14),
        .irqa(alpha_irqs[14]), .irqb(beta_irqs[14])
    );

    // I/O slot 15 - empty
    dummyio io15(
        .clk(clk), .rst(rst), .adr(bus_adr[7:0]), .adr_d1(bus_adr_d1[7:0]),
        .red(io_red15), .wrt(bus_wrt), .wen(io_wen && bus_adr[11:8] == 4'd15),
        .irqa(alpha_irqs[15]), .irqb(beta_irqs[15])
    );

    // I/O slot multiplexing
    always @*
        case (bus_adr_d1[11:8])
        4'd0: io_red = io_red0;
        4'd1: io_red = io_red1;
        4'd2: io_red = io_red2;
        4'd3: io_red = io_red3;
        4'd4: io_red = io_red4;
        4'd5: io_red = io_red5;
        4'd6: io_red = io_red6;
        4'd7: io_red = io_red7;
        4'd8: io_red = io_red8;
        4'd9: io_red = io_red9;
        4'd10: io_red = io_red10;
        4'd11: io_red = io_red11;
        4'd12: io_red = io_red12;
        4'd13: io_red = io_red13;
        4'd14: io_red = io_red14;
        4'd15: io_red = io_red15;
        endcase

    // program ROM, $c000 (or $e000 if shrunk to 8kB)
    // Actually, it might not be ROM at all, if you `define WRITEABLE_ROM.
    // It's really just RAM.  But in production code it's not usually going
    // to be written to.
`ifdef BIG_ROM
    reg [7:0]rom[0:16383];
    wire [13:0]rom_adr = bus_adr[13:0];
`else
    reg [7:0]rom[0:8191];
    wire [12:0]rom_adr = bus_adr[12:0];
`endif
    reg [7:0]rom_red = 8'd0;
    always @(posedge clk) begin
        rom_red <= rom[rom_adr];
`ifdef WRITEABLE_ROM
        if (write_rom && rom_wen) begin
            rom_red <= bus_wrt;
            rom[rom_adr] <= bus_wrt;
        end
`endif
    end
`ifdef BIG_ROM
    initial $readmemh("rom.mem", rom, 0, 16383);
`else
    initial $readmemh("rom.mem", rom, 0, 8191);
`endif

    // address decoding

    always @* begin
        ram_wen =  1'd0;
        rom_wen =  1'd0;
        text_wen = 1'd0;
        font_wen = 1'd0;
        io_wen =   1'd0;

        bus_red = 8'd0;
        casex (bus_adr[15:12])
        4'b0xxx: ram_wen =  bus_wen;    // $0000-$7fff - general memory
        4'b100x: text_wen = bus_wen;    // $8000-$9fff - text memory
        4'b1010: font_wen = bus_wen;    // $a000-$afff - font memory
        4'b1011: io_wen =   bus_wen;    // $b000-$bfff - memory mapped I/O
        4'b11xx: rom_wen =  bus_wen;    // $c000-$ffff - program memory
        endcase
        casex (bus_adr_d1[15:12])
        4'b0xxx: bus_red = ram_red;     // $0000-$7fff - general memory
        4'b100x: bus_red = text_red;    // $8000-$9fff - text memory
        4'b1010: bus_red = font_red;    // $a000-$afff - font memory
        4'b1011: bus_red = io_red;      // $b000-$bfff - memory mapped I/O
        4'b11xx: bus_red = rom_red;     // $c000-$ffff - program memory
        endcase
    end

    // audio output, a simple beep tone
    reg o_audio_l = 1'b0;
    assign o_audio_r = o_audio_l;
    always @(posedge clk)
        if (beep && masterbaud[13])
            o_audio_l <= ~o_audio_l;
endmodule
