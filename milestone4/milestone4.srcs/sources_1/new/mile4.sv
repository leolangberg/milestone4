/* TOP MODULE */
module top (
	input logic clk,
	input logic rst_n,
	input logic start,
	output logic [31:0] test_alu_out,
	output logic [31:0] test_sram_data_out,
	output logic [31:0] test_sram_data_in,
	output logic test_mem_ready,
	output logic [31:0] test_IR,
	output logic [4:0] test_PC,
	output logic [1:0] test_state,
	output logic test_sram_read_en
);


logic rf_chip_en;
logic rf_write_en_n;
logic sram_read_en;
logic sram_write_en;
logic sram_mem_ready;
logic load_IR;
logic load_PC;
logic alu_src;
logic alu_opcode_mux;
logic [6:0] ctrl_opcode;
logic [2:0] ctrl_func3;

controller CU (
	.clk(clk),
	.rst_n(rst_n),
	.start(start),
	.test_state(test_state),
	.test_sram_read_en(test_sram_read_en),

	.rf_chip_en(rf_chip_en),
	.rf_write_en_n(rf_write_en_n),
	.sram_read_en(sram_read_en),
	.sram_write_en(sram_write_en),
	.sram_mem_ready(sram_mem_ready),
	.ctrl_opcode(ctrl_opcode),
	.ctrl_func3(ctrl_func3),
	.alu_opcode_mux(alu_opcode_mux),
	.load_IR(load_IR),
	.load_PC(load_PC),
	.alu_src(alu_src)
);

datapath DP(
	.clk(clk),
	.rst_n(rst_n),
	.test_alu_out(test_alu_out),
	.test_mem_ready(test_mem_ready),
	.test_IR(test_IR),
	.test_PC(test_PC),
	.test_sram_data_out(test_sram_data_out),
	.test_sram_data_in(test_sram_data_in),

	.rf_chip_en(rf_chip_en),
	.rf_write_en_n(rf_write_en_n),
	.sram_read_en(sram_read_en),
	.sram_write_en(sram_write_en),
	.sram_mem_ready(sram_mem_ready),
	.ctrl_opcode(ctrl_opcode),
	.ctrl_func3(ctrl_func3),
	.alu_opcode_mux(alu_opcode_mux),
	.load_IR(load_IR),
	.load_PC(load_PC),
	.alu_src(alu_src)
	
);

endmodule;

typedef enum logic [6:0] { 
	RTYPE = 7'b0110011, 
	ITYPE = 7'b0010011
} instruction_opcode;

/* CONTROLLER CU */
module controller (
	input logic clk,
	input logic rst_n,
	input logic start,
	output logic [1:0] test_state,
	output logic test_sram_read_en,

	// Interface with Datapath
	output logic rf_chip_en,
	output logic rf_write_en_n,
	output logic sram_read_en,
	output logic sram_write_en,
	input logic sram_mem_ready,
	input instruction_opcode ctrl_opcode,	// [6:0] opcode.
	input logic [2:0] ctrl_func3,
	output logic alu_opcode_mux,
	output logic load_IR,
	output logic load_PC,
	output logic alu_src

);


typedef enum logic [2:0] { IDLE, INIT,  FETCH, EXECUTE } state;
state prev_state, next_state;

always_ff @(posedge clk or negedge rst_n) begin
    if(!rst_n) prev_state <= IDLE;
    else prev_state <= next_state;
end

always_comb begin
	// Initialization
	rf_chip_en = 0;
	rf_write_en_n = 0;
	sram_read_en = 0;
	sram_write_en = 0;
	alu_src = 0;
	alu_opcode_mux = 0;
	load_PC = 0;
	load_IR = 0;

	case (prev_state)
            IDLE: begin
                next_state = start ? INIT : IDLE;
            end

            INIT: begin
                next_state = start ? INIT : FETCH;
            end

            FETCH: begin
                rf_chip_en = 1;    
                sram_read_en = 1;
                if (sram_mem_ready == 1) begin
                    // Memory is ready, load instruction into IR
                    next_state = EXECUTE;
                     load_IR = 1;
                     load_PC = 1; // go to next instruction.     
             
                end else begin
                    // Wait for memory to be ready and read instruction
                    next_state = FETCH;
                end
            end
            EXECUTE: begin
                next_state = FETCH;
                rf_chip_en = 1;      
                rf_write_en_n = 0;   

		case (ctrl_opcode)
			RTYPE : begin
				// RTYPE instructions read only from register 
				// and use {func3, func7[5]}
				alu_opcode_mux 	= 1'b0;
				alu_src 	= 1'b0;
			end
			ITYPE : begin
				// ITYPE instructions read from imm
				// and use {func3, 0} (except for SLL, SRL, SRA)
				alu_opcode_mux 	= 1'b1;
                		alu_src 	= 1'b1;

				if((ctrl_func3 == 3'b001) || (ctrl_func3 == 3'b101))
					alu_opcode_mux = 1'b0;
			end
			default: begin
				// This case is for invalid opcodes (shutdown).
				// next_state = IDLE;
			end
		endcase
            end

            default: begin
                next_state = IDLE; 
            end
        endcase
end

assign test_state = prev_state;
assign test_sram_read_en = sram_read_en;

endmodule;


/* DATAPATH DP */
module datapath (
	input logic clk,
	input logic rst_n,

	// for testing output
	output logic [31:0] test_alu_out,
	output logic test_mem_ready,
	output logic [31:0] test_IR,
	output logic [31:0] test_sram_data_out,
	output logic [31:0] test_sram_data_in,
	output logic [4:0] test_PC,

	// Interface with Controller 
	input logic rf_chip_en,
	input logic rf_write_en_n,
	input logic sram_read_en,
	input logic sram_write_en,
	output logic sram_mem_ready,
	output logic [6:0] ctrl_opcode,
	output logic [2:0] ctrl_func3,
	input logic load_IR,
	input logic load_PC,
	input logic alu_src,
	input logic alu_opcode_mux
);


logic [31:0] PC; // PROGRAM COUNTER 
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
logic [31:0] 	sram_addr; // not sure about size.
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

	// Pass instruction parts to controller
	ctrl_opcode = opcode;
	ctrl_func3  = func3; 	

	/* LOGIC FOR I-TYPE INSTRUCTION */
	// Immidiate is sign extended and shifted to 32-bit value.
	imm	= {20'd0, IR[31:20]};
	
	/* REGISTER FILE CONNECTIONS */
	rf_write_addr 	= rd1;
	rf_read_addr_1 	= rs1;
	rf_read_addr_2  = rs2;
 
	/* ALU CONNECTIONS */
	// alu_opcode needs to ignore func7[5] for ITYPE instructions, we therefore
	// have a mux with signal from controller based on instruction opcode.
	// alu_src decides to read from register (R) or immediate (I).
	alu_opcode 	= (alu_opcode_mux) ? {func3, 1'b0} : {func3, func7[5]}; 
	alu_in_a 	= rf_data_out_1;
	alu_in_b 	= (alu_src) ? (imm) : (rf_data_out_2);
	rf_data_in 	= alu_out;
	
end;

assign test_alu_out = alu_out;
assign test_mem_ready = sram_mem_ready;
assign test_IR = IR;
assign test_PC = PC;
assign test_sram_data_out = sram_data_out;
assign test_sram_data_in = sram_data_in;

/* PROGRAM COUNTER */
// Holds Instruction pointer Register (IR)
// Updates PC = PC + 4 (increases addr).
always_ff @(posedge clk or negedge rst_n) begin
	if(!rst_n) begin
		PC <= '0;
	end else if (load_PC) begin
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

endmodule;
