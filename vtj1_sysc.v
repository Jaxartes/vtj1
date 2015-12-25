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

// vtj1_sysc.v
// "System controller" I/O device for "VTJ-1" project.  This sits in I/O
// slot zero and multiplexes 32 interrupt request lines.

module vtj1_sysc(
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
    input [15:0] alpha_irqs, // one "alpha IRQ" line per slot
    input [15:0] beta_irqs, // one "beta IRQ" line per slot
    output irq, // combined IRQ signal
    input [13:0] masterbaud // timing pulse trains for pulse rate generation
);
    // Interrupt enables
    reg [15:0] alpha_enas = 16'd0, beta_enas = 16'd0;

    // Configurably extract a clock signal from 'masterbaud'.
    // A clock generator, making use of the baud rate generation logic
    // designed for serial ports.
    reg [4:0] clkctl = 5'd0;
    wire clkb1;
    brg #(.BAUD8(0)) clkbrg(
        .clk(clk), .rst(rst), .masterbaud(masterbaud),
        .ctl(clkctl), .baudout(clkb1)
    );
    reg [1:0]clksig = 2'd0; // clksig[0] != clksig[1] when interrupt pending
    always @(posedge clk)
        if (rst)
            clksig[0] <= 1'd0;
        else if (clkb1)
            clksig[0] <= !clksig[1];

    // Priority encoder:  So that the software can easily see what the highest
    // priority interrupt it needs to handle is.
    wire [5:0] intpe;
    prienc32 pe(.ins({ beta_enas & beta_irqs, alpha_enas & alpha_irqs }),
                .out(intpe));

    // Combined and filtered IRQ
    assign irq = ~(intpe[5]);

    // Register access from CPU
    always @(posedge clk)
        if (rst) begin
            alpha_enas <= 16'd0;
            clksig[1] <= 1'd0;
        end else begin
            // reading registers
            red <= 8'd0; // dummy value for unpopulated registers
            case(adr[7:0])
            8'h00: red <= alpha_enas[7:0];
            8'h01: red <= alpha_enas[15:8];
            8'h08: red <= beta_enas[7:0];
            8'h09: red <= beta_enas[15:8];
            8'h40: red <= alpha_irqs[7:0];
            8'h41: red <= alpha_irqs[15:8];
            8'h48: red <= beta_irqs[7:0];
            8'h49: red <= beta_irqs[15:8];
            8'h80: red <= irq ? { { 2 { intpe[5] } }, intpe } : 8'd255;
            8'hc0: red <= { 3'd0, clkctl };
            endcase
            if (wen) begin
                red <= wrt;
                case (adr[7:0])
                8'h00: alpha_enas[7:0]  <= wrt;
                8'h01: alpha_enas[15:8] <= wrt;
                8'h08: beta_enas[7:0]   <= wrt;
                8'h09: beta_enas[15:8]  <= wrt;
                8'hc0: begin
                    clkctl <= wrt[4:0]; // set up baud rate generation
                    clksig[1] <= clksig[0]; // cancel pending interrupt
                end
                endcase
            end
        end

    // Our own interrupt signals.
    assign irqa = (clksig[0] != clksig[1]);
    assign irqb = 1'b0;
endmodule
