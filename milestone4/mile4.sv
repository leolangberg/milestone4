/* TOP MODULE */
/*
module top (
	input logic clk,
	input logic rst_n,
	
	
);

endmodule;
*/

/* CONTROLLER CU */
/*
module controller (

);
endmodule;
*/

/* DATAPATH DP */
module datapath (
	input clk,
	input rst_n,

);


logic [4:0] IRreg; // instruction pointer register
logic [31:0] instruction;

// ================================
// R-TYPE INSTRUCTION 
logic [6:0] rtype_opcode;
logic [4:0] rtype_rd1;
logic [2:0] rtype_func3;
logic [4:0] rtype_rs1;
logic [4:0] rtype_rs2;
logic [6:0] rtype_func7;
// ================================


/* Register File */
// 32-bit registers with 32 depth (5-bit addr).
// ================================
logic [31:0] 	rf_data_in;
logic [4:0] 	rf_read_addr_1; 
logic [4:0] 	rf_read_addr_2;
logic [4:0] 	rf_write_addr;
logic 		rf_write_en_n;
logic 		rf_chip_en;
logic [31:0] 	rf_data_out_1;
logic [31:0] 	rf_data_out_2;

RF #(.BW(32), .DEPTH(32)) rf(
	.clk(clk),
	.rst_n(rst_n),
	.data_in(rf_data_in),
	.read_addr_1(rf_read_addr_1),
	.read_addr_2(rf_read_addr_2),
	.write_addr(rf_write_addr),
	.write_en_n(rf_write_en_n),
	.chip_en(rf_chip_en),
	.data_out_1(rf_data_out_1),
	.data_out_2(rf_data_out_2)
);
// ================================


/* ALU */
// 32-bit bus.
// ================================
logic [31:0] 	alu_in_a;
logic [31:0] 	alu_in_b;
logic [3:0] 	alu_opcode; // 4-bit func3 & func7[5]
logic [31:0] 	alu_out;
logic [2:0] 	alu_flags;

ALU #(.BW(32)) alu(
	.in_a(alu_in_a),
	.in_b(alu_in_b),
	.opcode(alu_opcode),
	.out(alu_out),
	.flags(alu_flags)
);
// ================================


/* SRAM MEMORY */
// Address is updated either via IR (PC)
// or by "load" or "store" operations.
// ================================
logic [4:0] 	sram_addr; // not sure about size.
logic 		sram_read_en;
logic 		sram_write_en;
logic [31:0] 	sram_data_in;
logic [31:0] 	sram_data_out;
logic 		sram_mem_ready;

memory_system sram(
	.clk(clk),
	.rst_n(rst_n),
	.addr(sram_addr),
	.read_en(read_en),
	.write_en(write_en),
	.data_in(data_in),
	.data_out(data_out),
	.mem_ready(mem_ready)
);
// ================================


always_comb begin
	/* LOGIC FOR R-RTYPE INSTRUCTION */
	// 1. Parse rtype instruction
	// 2. [RD] = [RS1] + [RS2]
	sram_addr = IRreg;	// Progam Counter (PC)
	instruction = 		// collect instruction
	// PC updates itself. (+4)

	rtype_opcode 	= instruction[6:0];
	rtype_rd1	= instruction[11:7];
	rtype_func3	= instruction[14:12];
	rtype_rs1	= instruciton[19:15];
	rtype_rs2	= instruction[24:20];
	rtype_func7	= instruction[31:25];
	
	rf_write_addr 	= rtype_rd1;
	rf_read_addr_1 	= rtype_rs1;
	rf_read_addr_2  = rtype_rs2;

	alu_in_a = rf_data_out_1;
	alu_in_b = rf_data_out_2;
	alu_opcode = {func3, func7[5]}; 

	rf_data_in = alu_out;
	
	// Controll signals for RTYPE:
	write_en_n 	= 0;	// write yes
	chip_en		= 1;	// read/write yes	
	sram_read_en	= 1;
	
end;

/* PROGRAM COUNTER */
// Holds Instruction pointer Register (IR)
// Updates IR = IR + 4 (increases addr).
always_ff @(posedge clk or negedge rst_n) begin
	if(!rst_n) begin
		IRreg <= '0;
	end else begin
		IRreg <= IRreg + 4;
	end
end
endmodule;
