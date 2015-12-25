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

// vtj1_clock.v
// This module, vtj1_clock(), generates a clock signal of the appropriate
// frequency for VTJ-1 to use, based on whatever input the FPGA board has.
// This code will differ based on FPGA vendor and architecture and on the
// FPGA board.  It's controlled by various `defines found in vtj1-config.v,
// which is generated based on vtj1-config.txt, which see.

`include "vtj1-config.v"

// vtj1_clock(): Clock transformation.
module vtj1_clock(
    input iclk, // input clock, probably coming from an oscillator somewhere
    output oclk, // output clock, to be used by VTJ-1's logic
    output locked
);
`ifdef XILINX_SPARTAN_6
    // Generate a clock of the desired rate, using the Xilinx Spartan 6's
    // Clock Management Tile's PLL component.
    wire feedback; // feedback within the PLL

    wire oclkb;
    BUFG outbuf (.I(oclkb), .O(oclk));

    PLL_BASE #(

`ifdef INPUT_CLOCK_32MHZ
        // Utilize a 32MHz input clock as found on the Papilio Pro board
        .CLKIN_PERIOD(31.250), // 31.250 ns means 32MHz, input clock frequency
`ifdef VTJ1_CLOCK_25MHZ
        .DIVCLK_DIVIDE(1), // feed 32MHz to the PLL input
        .CLKFBOUT_MULT(25), // run the VCO at 32*25=800MHz (ideal seems to
                            // be a little over 400MHz)
        .CLKOUT0_DIVIDE(32) // and give output 0, 800MHZ / 32 = 25MHz
`endif // VTJ1_CLOCK_25MHZ
`ifdef VTJ1_CLOCK_50MHZ
        .DIVCLK_DIVIDE(1), // feed 32MHz to the PLL input
        .CLKFBOUT_MULT(25), // run the VCO at 32*25=800MHz (ideal seems to
                            // be a little over 400MHz)
        .CLKOUT0_DIVIDE(16) // and give output 0, 800MHZ / 16 = 50MHz
`endif // VTJ1_CLOCK_50MHZ
`ifdef VTJ1_CLOCK_51200KHZ
        // A bit higher than 2x the pixel clock, but for some displays that
        // might be a good thing.
        .DIVCLK_DIVIDE(1), // feed 32MHz to the PLL input
        .CLKFBOUT_MULT(16), // run the VCO at 32*16=512MHz (ideal seems to
                            // be a little over 400MHz)
        .CLKOUT0_DIVIDE(10) // and give output 0, 512MHz / 10 = 51.2MHz
`endif // VTJ1_CLOCK_51200KHZ
`ifdef VTJ1_CLOCK_74667KHZ
        // 74.667 MHz clock: approx 3x the pixel rate
        .DIVCLK_DIVIDE(1), // feed that 32MHz to the PLL input
        .CLKFBOUT_MULT(14), // run the VCO at 32*14=448MHz (ideal seems to
                            // be a little over 400MHz)
        .CLKOUT0_DIVIDE(6) // and give output 0, 448MHZ / 6 = 74.667MHz
`endif // VTJ1_CLOCK_74667KHZ
`ifdef VTJ1_CLOCK_76800KHZ
        // 76.8 MHz clock: also approx 3x the pixel rate; not as close
        //                 an approximation as 74.667MHz, but being high,
        //                 perhaps better for some displays
        .DIVCLK_DIVIDE(1), // feed that 32MHz to the PLL input
        .CLKFBOUT_MULT(24), // run the VCO at 32*24=768MHz (ideal seems to
                            // be a little over 400MHz)
        .CLKOUT0_DIVIDE(10) // and give output 0, 768MHz / 10 = 76.8MHz
`endif // VTJ1_CLOCK_76800KHZ
`endif // INPUT_CLOCK_32MHz
    ) mainpll (
        .CLKIN(iclk), // input clock
        .CLKOUT0(oclkb), // output clock
        .CLKFBIN(feedback), // feedback to keep the PLL running
        .CLKFBOUT(feedback),
        .LOCKED(locked), // indication of the PLL's status
        .RST(0) // I guess you need to keep it from resetting itself...
    );
`endif // XILINX_SPARTAN_6
endmodule
