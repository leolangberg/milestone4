`timescale 1ns / 1ps
module dp_tb;

logic clk;
logic rst_n;
logic [31:0] rf_data_out_1;
logic [31:0] rf_data_out_2;
logic [31:0]alu_out;

datapath mile4_tb(
.clk(clk),
.rst_n(rst_n),
.rf_data_out_1(rf_data_out_1),
.rf_data_out_2(rf_data_out_2),
.alu_out(alu_out)
);

initial begin
clk =0;
forever #5   clk = ~clk;
end
initial begin
rst_n = 1'd0;
#10
rst_n = 1'd1;
end

endmodule


