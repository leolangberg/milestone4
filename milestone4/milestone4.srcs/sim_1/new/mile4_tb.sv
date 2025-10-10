`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 10/09/2025 03:07:32 PM
// Design Name: 
// Module Name: mile4_tb
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


module mile4_tb;

logic clk;
logic rst_n;
logic start;
logic [31:0] test_alu_out;
logic test_mem_ready;
logic [31:0] test_IR;
logic [1:0] test_state;
logic [31:0] test_sram_data_out;
logic [31:0] test_sram_data_in;
logic [4:0] test_PC;
logic test_sram_read_en;

top DUT(
    .clk(clk),
    .rst_n(rst_n),
    .start(start),
    .test_alu_out(test_alu_out),
    .test_mem_ready(test_mem_ready),
    .test_IR(test_IR),
    .test_PC(test_PC),
    .test_state(test_state),
    .test_sram_data_out(test_sram_data_out),
    .test_sram_data_in(test_sram_data_in),
    .test_sram_read_en(test_sram_read_en)
);

initial begin
    clk = 0;
    forever #5 clk = ~clk;
end

initial begin
    rst_n = 0;
    @(posedge clk)
    // instruction pointer 0x00000004
    // addi rd, rs1, imm
    rst_n = 1;
    @(posedge clk);
    start = 1;
    @(posedge clk);
    start = 0;
    @(posedge clk);
    
end
endmodule


