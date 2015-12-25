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

// vtj1_ps2.v
// PS/2 keyboard/mouse port I/O device for "VTJ-1" project.
// This fills in one of the eight serial-port I/O slots, presenting a memory
// mapped register interface to the 6502 CPU and in turn controlling a PS/2
// keyboard/mouse port.

// vtj1_ps2() - main module including external interface
module vtj1_ps2(
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
    inout ps2clk, // PS/2 port's "clock" line
    inout ps2dat, // PS/2 port's "data" line
    input sixus // a single cycle clock pulse every six microseconds
);
    // 1-byte FIFO for transmitting bytes (TX)
    reg [7:0]txf_data = 8'd0; // the byte data
    reg [1:0]txf_sema = 2'd0; // these bits are != if there's a byte in there
    // YYY consider getting rid of this shift register, not needed

    // 1-byte FIFO for receiving bytes (RX)
    reg [7:0]rxf_data = 8'd0; // the data
    reg [1:0]rxf_sema = 2'd0; // these bits are != if there's a byte in there

    // Exception indicators.  Each is two bits, if they're different the
    // exception has occurred, if not it hasn't, or has been cleared.
    reg [1:0] exc_rxoflow = 2'd0, exc_rxparity = 2'd0, exc_txbadack = 2'd0;
    // YYY the stuff involving these exception indicators is probably
    // not going to get tested unless someone actually makes a fake keyboard
    // side or something.

    // Bus interface with control registers mapped into 6502's address space
    always @(posedge clk)
        if (rst) begin
            txf_data <= 8'd0;
            txf_sema[0] <= 1'b0;
            rxf_sema[1] <= 1'b0;
            exc_rxoflow[0] <= 1'b0;
            exc_rxparity[0] <= 1'b0;
            exc_txbadack[0] <= 1'b0;
        end else begin
            red <= 8'd0;
            case(adr[2:0])
            3'd1: begin // transmit a byte
                // Put it on the 1-byte FIFO; it's up to the software
                // to check that it's empty first, or bytes will be lost.
                if (wen) begin
                    txf_data <= wrt;
                    txf_sema[0] <= ~txf_sema[1];
                end
            end
            3'd2: begin // receive a byte
                red <= rxf_data[7:0];
                if (wen) begin
                    // On write, remove the byte from the FIFO.
                    rxf_sema[1] <= rxf_sema[0];
                end
            end
            3'd3: begin // device status
                red <= { exc_rxparity[0] != exc_rxparity[1],
                         exc_rxoflow[0]  != exc_rxoflow[1],
                         exc_txbadack[0] != exc_txbadack[1],
                         5'd0 };
                if (wen) begin
                    // writing a '1' bit over an exception clears it
                    if (wrt[7]) exc_rxparity[0] <= exc_rxparity[1];
                    if (wrt[6]) exc_rxoflow[0]  <= exc_rxoflow[1];
                    if (wrt[5]) exc_txbadack[0] <= exc_txbadack[1];
                end
            end
            3'd4: // debugging access (YYY grot)
                red <= rxsr[7:0];
            3'd5: // debugging access (YYY grot)
                red <= { 6'd0, rxsr[9:8] };
            endcase
            if (wen) red <= wrt;
        end

    assign irqa = (rxf_sema[0] != rxf_sema[1]); // is FIFO full
    assign irqb = (txf_sema[0] == txf_sema[1]); // is FIFO empty

    // Now oversample the data & clock lines & take the average of 5 samples.
    // And take 2 more samples for synchronization.
    reg [6:0] datosa = 7'd0, clkosa = 7'd0;
    always @(posedge clk)
        if (rst) begin
            datosa <= 7'd0;
            clkosa <= 7'd0;
        end else if (sixus) begin
            datosa <= { datosa[5:0], ps2dat };
            clkosa <= { clkosa[5:0], ps2clk };
        end
    wire datin, clkin;
    avg5 datavg(datin, datosa[6:2]);
    avg5 clkavg(clkin, clkosa[6:2]);

    // detect rising edge of clkin
    reg clkold = 1'b0;
    always @(posedge clk)
        if (rst)
            clkold <= 1'b0;
        else if (sixus)
            clkold <= clkin;
    wire clkfall = clkold && !clkin;

    // RX: sample the data line ~18us after the clock goes
    // low.  This code shifts it into an 11-bit shift register.  This
    // also takes an input signal, suppress_rx, so that we don't
    // "receive" the signals we send.
    // When the "start bit" makes its way through 'rxsr' we know that the
    // byte is in there and we see about putting it on the FIFO.
    // The byte is accepted whether or not its parity is valid.
    // An exception flag is flipped if the parity is not valid or if
    // there's no stop bit.
    reg [9:0] rxsr = 10'd1023;
    reg [2:0] xtrig = 3'd0; // timing after the clock goes low
    wire suppress_rx;
    always @(posedge clk)
        if (rst) begin
            xtrig <= 3'd0;
            rxsr <= 10'd1023;
            exc_rxparity[1] <= 1'b0; // clear exception
            exc_rxoflow[1]  <= 1'b0; // clear exception
            rxf_sema[0] <= 1'b0; // empty FIFO
        end else if (sixus) begin
            if (xtrig[2] && !suppress_rx) begin
                if (rxsr[0])
                    // not a byte yet; just shift the bit into the register
                    rxsr <= { datin, rxsr[9:1] };
                else begin
                    // received a byte:
                    //      rxsr[0] holds the start bit (0)
                    //      rxsr[1-8] hold the data byte, LSbit first
                    //      rxsr[9] holds the parity bit (odd parity)
                    //      datin holds the stop bit (1)
                    // clear the shift register
                    rxsr <= { datin, 9'd511 };
                    // check parity & stop bit
                    if ((~datin) || !(^(rxsr[9:1])))
                        exc_rxparity[1] <= !(exc_rxparity[0]);
                    // put it in the FIFO
                    rxf_data <= rxsr[8:1];
                    // record that the FIFO is full, if it wasn't already
                    rxf_sema[0] <= !(rxf_sema[1]);
                    if (rxf_sema[0] != rxf_sema[1])
                        // an overflow exception has occurred
                        exc_rxoflow[1] <= !(exc_rxoflow[0]);
                end
            end
            xtrig <= clkfall ? 3'd1 : { xtrig[1:0], 1'b0 };
        end

    // TX: Transmission of bytes from the host to the peripheral.  Not as
    // "important" as RX, and largely unnecessary for keyboards, but
    // handy to be able to do.

    // txsr is a shift register with what we're transmitting incl parity
    //      and a '1' start bit
    // txctr is a state variable:
    //      0 - not transmitting
    //      1 - finishing transmission (acknowledgement bit from device)
    //      2-11 - 1 plus number of bits not yet transmitted
    //      12-31 - counts 6us periods for starting communication
    reg [9:0] txsr = 10'd1023;
    reg [4:0] txctr = 5'd0;
    wire tx_going = |txctr; // indicates a TX operation is happening
    assign suppress_rx = tx_going; // no RX while any TX is happening
    wire tx_initiate = txctr >= 12; // pull ps2clk down to initiate
    wire tx_transmit = txctr >= 2 && txctr <= 11; // operate ps2dat
    wire tx_waitack = txctr == 1; // expecting acknowledgement
    wire [4:0] txctr_minus_1 = txctr - 1;
    always @(posedge clk)
        if (rst) begin
            txsr <= 10'd1023; // start with nothing to transmit
            txctr <= 5'd0; // start with not transmitting
            txf_sema[1] <= 1'd0; // start with nothing in transmit FIFO
            exc_txbadack[1] <= 1'd0; // clear exception
        end else if (tx_initiate && sixus)
            txctr <= txctr_minus_1; // countdown to start
        else if (tx_transmit && sixus && xtrig[2]) begin
            // transmitting a bit
            txctr <= txctr_minus_1; // countdown to next bit
            txsr <= { 1'b1, txsr[9:1] }; // shift out a bit
        end else if (tx_waitack && sixus && xtrig[2]) begin
            // acknowledgement bit
            txctr <= txctr_minus_1;
            if (datin) begin
                // back ACK bit received
                exc_txbadack[1] <= !(exc_txbadack[1]);
            end
        end else if ((txf_sema[0] != txf_sema[1]) && !tx_going) begin
            // we have a byte to transmit: start transmitting
            txctr <= 5'd31; // start the countdown
            txsr <= { ~(^txf_data), txf_data, 1'b0 }; // fill shift register
            txf_sema[1] <= txf_sema[0]; // empty the FIFO
        end
    assign ps2clk = tx_initiate ? 1'b0 : 1'bz;
    assign ps2dat = (tx_transmit && !txsr[0]) ? 1'b0 : 1'bz;
endmodule

// avg5() -- average of 5 input bits.  Should compact into a LUT5 (half
// a LUT) on Spartan6.  I'm not sure it does, but it seems ok to me.
module avg5(output o, input [4:0] i);
    assign o = ((i[0] ? 1 : 0) +
                (i[1] ? 1 : 0) +
                (i[2] ? 1 : 0) +
                (i[3] ? 1 : 0) +
                (i[4] ? 1 : 0)) > 2;
endmodule
