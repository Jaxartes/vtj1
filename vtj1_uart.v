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

// vtj1_uart.v
// Asynchronous serial port I/O device for "VTJ-1" project.
// This fills in one of the eight serial-port I/O slots, presenting a memory
// mapped register interface to the 6502 CPU and in turn controlling a serial
// port.  "UART" is the customary name for these things, but in this case
// it's a misnomer:  This one is far from "universal."

`define ENABLE_RXOFLOW 1 // enables a 1-byte counter of RX buffer overflows

// vtj1_uart() - main module including external interface
module vtj1_uart(
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
    input rx_raw, // raw RX line within the serial port
    output tx, // raw TX line within the serial port
    input [13:0] masterbaud // chains of clock pulses
);
    // 1-byte FIFO for transmitting bytes (TX)
    reg [7:0]txf_data = 8'd0; // the byte data
    reg [1:0]txf_sema = 2'd0; // these bits are != if there's a byte in there

    // 1-byte FIFO for receiving bytes (RX)
    // Has an additional bit to encode exceptions.
    reg [8:0]rxf_data = 9'd0; // the data
    reg [1:0]rxf_sema = 2'd0; // these bits are != if there's a byte in there

`ifdef ENABLE_RXOFLOW
    reg [7:0] rxf_oflows = 8'd0; // counter of RX buffer overflows
`endif

    // Bus interface with control registers mapped into 6502's address space
    reg [4:0] txbaud_ctl = 5'd31, rxbaud_ctl = 5'd31;
    reg extrastop_ctl = 1'd1;
    reg txbreak_ctl = 1'd0;
    always @(posedge clk)
        if (rst) begin
            txbaud_ctl <= 5'd31;
            rxbaud_ctl <= 5'd31;
            extrastop_ctl <= 1'd1;
            txbreak_ctl <= 1'd0;
            txf_data <= 8'd0;
            txf_sema[0] <= 1'b0;
            rxf_sema[1] <= 1'b0;
        end else begin
            red <= 8'd0;
            case(adr[2:0])
`ifdef ENABLE_RXOFLOW
            3'b000: begin // report RX buffer overflows
                red <= rxf_oflows;
            end
`endif
            3'b001: begin // transmit a byte
                // Put it on the 1-byte FIFO; it's up to the software
                // to check that it's empty first, or bytes will be lost.
                if (wen) begin
                    txf_data <= wrt;
                    txf_sema[0] <= ~txf_sema[1];
                end
            end
            3'b010: begin // receive a byte
                red <= rxf_data[7:0];
                if (wen) begin
                    // On write, remove the byte from the FIFO.
                    rxf_sema[1] <= rxf_sema[0];
                end
            end
            3'b011: // device status (read-only)
                red <= {
                    7'd0, // unused bits
                    rxf_data[8] // indicate exception
                };
            3'b100: begin // control byte A for transmit
                red <= { 3'd0, txbaud_ctl };
                if (wen) txbaud_ctl <= wrt[4:0];
            end
            3'b101: begin // control byte B for transmit
                red <= {
                    1'd0, // this field not defined
                    txbreak_ctl, // transmit a break
                    1'd1, // 7-bit bytes not supported
                    extrastop_ctl, // transmit extra stop bit
                    1'd0, // this field not defined
                    3'd0 // parity not supported
                };
                if (wen) extrastop_ctl <= wrt[4];
                if (wen) txbreak_ctl <= wrt[6];
            end
            3'b110: begin // control byte A for receive
                red <= { 3'd0, rxbaud_ctl };
                if (wen) rxbaud_ctl <= wrt[4:0];
            end
            3'b111: begin // control byte B for receive
                red <= {
                    2'd0, // these fields not defined
                    1'd1, // 7-bit bytes not supported
                    2'd0, // these fields not defined
                    3'd0 // parity not supported
                };
            end
            default: red <= 8'd0; // unimplemented
            endcase
            if (wen) red <= wrt;
        end

    assign irqa = (rxf_sema[0] != rxf_sema[1]); // is FIFO full
    assign irqb = (txf_sema[0] == txf_sema[1]); // is FIFO empty

    // Baud rate generation: separately for transmit & receive
    wire txbaud1; // for TX, single cycle pulses at the baud rate
    wire txbaud8, rxbaud8; // single cycle pulses at 8x baud rate
    brg #(.BAUD8(0)) txbrg(
        .clk(clk), .rst(rst), .masterbaud(masterbaud),
        .ctl(txbaud_ctl), .baudout(txbaud1)
    );
    brg #(.BAUD8(1)) rxbrg(
        .clk(clk), .rst(rst), .masterbaud(masterbaud),
        .ctl(rxbaud_ctl), .baudout(rxbaud8)
    );

    // Transmit data.
    // Transmitted from a shift register in the following order:
    //      + start bit (0)
    //      + data bits from least-order
    //      + parity bit if any
    //      + stop bit(s) (1, same as idle)
    // Signals:
    //      txsr - shift register for transmit, shifts left to right
    //      txctr - counts bits left to transmit from txsr
    //      txempty - indicates 'txsr' is empty
    reg [8:0] txsr = 9'd511;
    reg [3:0] txctr = 4'd0;
    wire txempty = ~|txctr;
    always @(posedge clk)
        if (rst) begin
            // On reset: txsr will be empty & so will the FIFO.
            txsr <= 9'd511;
            txctr <= 4'd0;
            txf_sema[1] <= 1'b0;
        end else if (txbreak_ctl) begin
            // Transmit a "break" as long as txbreak_ctl is high, and
            // a little bit longer.  Also clobbers anything that's been
            // queued for transmission, in the FIFO or the shift register.
            txsr <= 9'd0;
            txctr <= 4'd15;
            txf_sema[1] <= txf_sema[0];
        end else if (txbaud1) begin
            // Time to transmit another bit.  From txsr if empty, otherwise
            // from the FIFO.  If there aren't other bits we'll just
            // transmit '1' until we get some.
            if (txctr > 4'd1) begin
                // txsr contains more than one bit; shift one out & transmit
                // the next
                txsr <= { 1'b1, txsr[8:1] };
                txctr <= txctr - 4'd1;
            end else if (txf_sema[0] != txf_sema[1]) begin
                // Take a new byte off the FIFO, because txsr has become
                // empty (if it wasn't already).
                txsr <= { 
                    // Stop bit(s) are left out of txsr, since they're
                    // the same as idle.
                    txf_data, // data byte (8 bits)
                    1'd0 // start bit
                };
                txctr <= extrastop_ctl ?
                    4'd11 : // 1 start, 8 data, 2 stop
                    4'd10;  // 1 start, 8 data, 1 stop
                txf_sema[1] <= txf_sema[0];
            end else begin
                // the shift register and FIFO are empty
                txsr <= 9'd511;
                txctr <= 4'd0;
            end
        end

    assign tx = txsr[0];

    // RX is more complicated than TX, because of oversampling and detecting
    // the start of a byte.

    // Start out by synchronizing the rx signal to our clock.
    reg rx_sync = 1'b1, rx = 1'b1;
    always @(posedge clk)
        if (rst)
            { rx, rx_sync } <= 2'b1;
        else
            { rx, rx_sync } <= { rx_sync, rx_raw };

    // Follow that by sampling at 8x the baud rate, and taking the
    // average of three adjacent samples.
    reg [2:0]rx_sample = 3'd1;
    always @(posedge clk)
        if (rst)
            rx_sample <= 3'd1;
        else if (rxbaud8)
            rx_sample <= { rx_sample[1:0], rx };
    reg rx_avg;
    always @*
        casex (rx_sample)
        3'b00x: rx_avg <= 1'b0;
        3'b11x: rx_avg <= 1'b1;
        default: rx_avg <= rx_sample[0];
        endcase

    // Now pull that into a shift register that holds all the samples
    // we'll need to receive a byte.  It shifts from left to right (so
    // rxsr[0] is the earliest) and looks as follows when a byte has
    // been received:
    //      rxsr[0] - before the start bit: 1
    //      rxsr[8:1] - start bit: 0; value taken at rxsr[4]
    //      rxsr[16:9] - least-order data bit
    //          ...
    //      rxsr[72:65] - greatest-order data bit
    // If we have parity or 7-bit data that will change of course.
    // We don't check for the stop bits, except for the single sample
    // that comes right before the next start bit.
    reg [72:0]rxsr = { 73{1'b1} };
    always @(posedge clk)
        if (rst) begin
            rxsr <= { 73{1'b1} };
`ifdef ENABLE_RXOFLOW
            rxf_oflows <= 8'd0;
`endif
        end else if (rxbaud8) begin
            if (rxsr[4:0] == 5'd1) begin
                // a byte has been received
`ifdef ENABLE_RXOFLOW
                if (rxf_sema[0] != rxf_sema[1]) begin
                    // there was already a byte in the RX FIFO;
                    // it's lost now; count it for debugging
                    rxf_oflows <= rxf_oflows + 8'd1;
                end
`endif
                rxf_data <= {
                    1'b0, // a byte, not an exception
                    rxsr[68], rxsr[60], rxsr[52], rxsr[44],
                    rxsr[36], rxsr[28], rxsr[20], rxsr[12]
                };
                rxf_sema[0] <= ~rxf_sema[1];
                rxsr <= { rx_avg, { 72{1'b1} } };
            end else begin
                // just another sample
                if (rxsr[72] && ~|rxsr[71:0]) begin
                    // but we're just coming out of a "break"
                    rxf_data <= {
                        1'b1, // an exception, not a byte
                        8'd0 // code for a "break"
                    };
                    // YYY I suspect this is a little wrong
                end
                rxsr <= { rx_avg, rxsr[72:1] };
            end
        end
        
endmodule
