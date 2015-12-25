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

// vtj1_gpio.v
// I/O device for "VTJ-1" project providing up to eight LEDs and up to
// eight buttons, along with some other fairly simple signals.

module vtj1_gpio(
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
    input [(NBTN-1):0] raw_btns, // raw button inputs
    output reg [(NLED-1):0] leds, // LED outputs
    output reg write_rom, // make the program memory ("rom") writeable
                          // if supported
    output reg beep, // turn on the "beep" if supported
    input dimctl // When this is high, suppresses all the LED output.
                 // Use it if you want to dim them, with an input with
                 // the appropriate duty cycle and frequency.
);
    parameter NBTN = 4'd4;
    parameter NLED = 4'd5;

    // we have no IRQs
    assign irqa = 1'b0;
    assign irqb = 1'b0;

    reg [(NBTN-1):0] btnsyn, btns;
    reg [(NLED-1):0] ledsr;

    genvar i;
    generate
        for (i = 0; i < NBTN; i = i + 1) begin : btnsync
            always @(posedge clk) begin
                // convert the asynchronous 'raw_btns' signals into
                // synchronous 'btns' ones
                btnsyn[i] <= raw_btns[i];
                btns[i] <= btnsyn[i];
            end
        end
    endgenerate

    // register access
    always @(posedge clk)
        if (rst) begin
            red <= 8'd0;
            ledsr <= 8'd0;
            write_rom <= 1'b0;
        end else begin
            casex (adr)
            8'b0???_??00: red <= { 4'd0, NLED }; 
            8'b0???_??01: red <= { 4'd0, NBTN };
            8'b1???_??00: red <= ledsr;
            8'b1???_??01: red <= btns;
            endcase
            if (wen) begin
                red <= wrt;
                casex (adr)
                8'b1???_??00: ledsr <= wrt;
                8'b1???_??10:
                    case(wrt[4:0])
                    5'd10: write_rom <= 1'b1;
                    5'd15: beep <= 1'b1;
                    5'd20: write_rom <= 1'b0;
                    5'd25: beep <= 1'b0;
                    endcase
                endcase
            end
        end

    // LED output
    always @(posedge clk)
        if (rst)
            leds <= 8'd0;
        else if (dimctl)
            leds <= 8'd0;
        else
            leds <= ledsr;

endmodule
