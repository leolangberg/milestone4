/* TOP MODULE */
module top (
	input logic clk,
	input logic rst_n,
	input logic start,
	output logic test_alu_out
);


logic rf_chip_en;
logic rf_write_en_n;
logic sram_read_en;
logic sram_write_en;
logic sram_mem_ready;
logic load_IR;
logic alu_src;

controller CU (
	.clk(clk),
	.rst_n(rst_n),
	.start(start),

	.rf_chip_en(rf_chip_en),
	.rf_write_en_n(rf_write_en_n),
	.sram_read_en(sram_read_en),
	.sram_write_en(sram_write_en),
	.sram_mem_ready(sram_mem_ready),
	.load_IR(load_IR),
	.alu_src(alu_src)
);

datapath DP(
	.clk(clk),
	.rst_n(rst_n),
	.test_alu_out(test_alu_out),

	.rf_chip_en(rf_chip_en),
	.rf_write_en_n(rf_write_en_n),
	.sram_read_en(sram_read_en),
	.sram_write_en(sram_write_en),
	.sram_mem_ready(sram_mem_ready),
	.load_IR(load_IR),
	.alu_src(alu_src)
	
);

endmodule;

/* CONTROLLER CU */
module controller (
	input logic clk,
	input logic rst_n,
	input logic start,

	// Interface with Datapath
	output logic rf_chip_en,
	output logic rf_write_en_n,
	output logic sram_read_en,
	output logic sram_write_en,
	input logic sram_mem_ready,
	output logic load_IR,
	output logic alu_src

);


typedef enum logic [1:0] { IDLE, INIT,  FETCH, EXECUTE1 } state;
state prev_state, next_state;

always_comb begin
	// Initialization
	rf_chip_en = 0;
	rf_write_en_n = 0;
	sram_read_en = 0;
	sram_write_en = 0;
	alu_src = 0;

	case (prev_state) 
		IDLE: next_state = start ? INIT : IDLE;
		INIT: next_state = start ? INIT : FETCH;
	
		FETCH: begin
			if(sram_mem_ready == 1) begin
				// Wait until memory is ready then go to next state.
				// load_IR is 1 which means IR register will now hold instruction.
				next_state = EXECUTE;
				load_IR = 1;
			end else begin
				// When memory is ready load instruction in Instr. Reg. (IR)
				// This is done by setting sram_read_en here.
				// We ask "is mem ready" and it returns 1 when ready.
				sram_read_en = 1;
				next_state = FETCH;
			end

		EXECUTE1: begin
			next_state = FETCH;
			rf_chip_en = 1;
			rf_write_en_n = 1;
			alu_src = 1;	
		
		end
		default: next_state = IDLE;
	endcase
end

endmodule;

/* DATAPATH DP */
module datapath (
	input logic clk,
	input logic rst_n,

	// for testing output
	output logic test_alu_out,

	// Interface with Controller 
	input logic rf_chip_en,
	input logic rf_write_en_n,
	input logic sram_read_en,
	input logic sram_write_en,
	output logic sram_mem_ready,
	input logic load_IR,
	input logic alu_src
);


logic [4:0] PC; // PROGRAM COUNTER 
logic [31:0] IR; // INSTRUCTION REGISTER

// ================================
// R-TYPE INSTRUCTION 
logic [6:0] opcode;
logic [4:0] rd1;
logic [2:0] func3;
logic [4:0] rs1;
logic [4:0] rs2;
logic [6:0] func7;
// ================================
// I-TYPE INSTRUCTION 
logic [11:0] imm;
// ================================


/* Register File */
// 32-bit registers with 32 depth (5-bit addr).
// ================================
logic [31:0] 	rf_data_in;
logic [4:0] 	rf_read_addr_1; 
logic [4:0] 	rf_read_addr_2;
logic [4:0] 	rf_write_addr;
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
logic [31:0] 	sram_data_in;
logic [31:0] 	sram_data_out;

memory_system sram(
	.clk(clk),
	.rst_n(rst_n),
	.addr(sram_addr),
	.read_en(sram_read_en),
	.write_en(sram_write_en),
	.data_in(sram_data_in),
	.data_out(sram_data_out),
	.mem_ready(sram_mem_ready)
);
// ================================


always_comb begin

	sram_addr = PC;	// Progam Counter (PC)
	// PC updates itself. (+4)

	/* LOGIC FOR R-RTYPE INSTRUCTION */
	// 1. Parse rtype instruction
	// 2. [RD] = [RS1] + [RS2]
	opcode 	= IR[6:0];
	rd1	= IR[11:7];
	func3	= IR[14:12];
	rs1	= IR[19:15];
	rs2	= IR[24:20];
	func7	= IR[31:25];

	/* LOGIC FOR I-TYPE INSTRUCTION */
	// Immidiate is sign extended and shifted to 32-bit value.
	imm	= {20'd0, IR[31:20]};
	
	/* REGISTER FILE CONNECTIONS */
	rf_write_addr 	= rd1;
	rf_read_addr_1 	= rs1;
	rf_read_addr_2  = rs2;
 
	/* ALU CONNECTIONS */
	// alu_src decides to read from register (R) or immediate (I).
	alu_opcode 	= {func3, func7[5]}; 
	alu_in_a 	= rf_data_out_1;
	alu_in_b 	= (alu_src) ? (imm) : (rf_data_out_2);
	rf_data_in 	= alu_out;
	
end;

assign test_alu_out = alu_out;

/* PROGRAM COUNTER */
// Holds Instruction pointer Register (IR)
// Updates PC = PC + 4 (increases addr).
always_ff @(posedge clk or negedge rst_n) begin
	if(!rst_n) begin
		PC <= '0;
	end else begin
		PC <= PC + 4;
	end
end

/* INSTRUCTION REGISTER (IR) */
always_ff @(posedge clk or negedge rst_n) begin
	if(!rst_n) 
		IR <= '0;
	else if (load_IR)
		IR <= sram_data_out;
	end
end

endmodule;
