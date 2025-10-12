/* TOP MODULE */
module top (
	input logic clk,
	input logic rst_n,
	input logic start
);

logic rf_chip_en;
logic rf_write_en_n;
logic sram_read_en;
logic sram_write_en;
logic [2:0] sram_read_mask_mux;
logic [1:0] sram_addr_mux;
logic sram_mem_ready;
logic load_IR;
logic load_PC;
logic [1:0] alu_src;
logic alu_opcode_mux;
logic [6:0] ctrl_opcode;
logic [2:0] ctrl_func3;
logic [1:0] rf_data_in_mux;

controller CU (
	.clk(clk),
	.rst_n(rst_n),
	.start(start),

	.rf_chip_en(rf_chip_en),
	.rf_write_en_n(rf_write_en_n),
	.sram_read_en(sram_read_en),
	.sram_write_en(sram_write_en),
	.sram_read_mask_mux(sram_read_mask_mux),
	.sram_addr_mux(sram_addr_mux),
	.sram_mem_ready(sram_mem_ready),
	.ctrl_opcode(ctrl_opcode),
	.ctrl_func3(ctrl_func3),
	.load_IR(load_IR),
	.load_PC(load_PC),
	.alu_opcode_mux(alu_opcode_mux),
	.alu_src(alu_src),
	.rf_data_in_mux(rf_data_in_mux)
	
);

datapath DP(
	.clk(clk),
	.rst_n(rst_n),

	.rf_chip_en(rf_chip_en),
	.rf_write_en_n(rf_write_en_n),
	.sram_read_en(sram_read_en),
	.sram_write_en(sram_write_en),
	.sram_read_mask_mux(sram_read_mask_mux),
	.sram_addr_mux(sram_addr_mux),
	.sram_mem_ready(sram_mem_ready),
	.ctrl_opcode(ctrl_opcode),
	.ctrl_func3(ctrl_func3),
	.load_IR(load_IR),
	.load_PC(load_PC),
	.alu_opcode_mux(alu_opcode_mux),
	.alu_src(alu_src),
	.rf_data_in_mux(rf_data_in_mux)
	
);

endmodule;

typedef enum logic [6:0] { 
	RTYPE 		= 7'b0110011, 
	ITYPEIMM	= 7'b0010011,
	ITYPELOAD	= 7'b0000011,
	UTYPELUI	= 7'b0110111,
	UTYPEAUIPC	= 7'b0010111
} instruction_opcode;

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
	output logic [2:0] sram_read_mask_mux,
	output logic [1:0] sram_addr_mux,
	input logic sram_mem_ready,
	input instruction_opcode ctrl_opcode,	// [6:0] opcode.
	input logic [2:0] ctrl_func3,
	output logic load_IR,
	output logic load_PC,
	output logic [1:0] alu_src,
	output logic alu_opcode_mux,
	output logic [1:0] rf_data_in_mux

);


typedef enum logic [2:0] { IDLE, INIT,  FETCH, EXECUTE1, EXECUTE2} state;
state prev_state, next_state;

always_ff @(posedge clk or negedge rst_n) begin
    if(!rst_n) prev_state <= IDLE;
    else prev_state <= next_state;
end

always_comb begin
	// Initialization
	rf_chip_en 		= 0;
	rf_write_en_n 		= 0;
	sram_read_en 		= 0;
	sram_write_en 		= 0;
	sram_read_mask_mux 	= 3'b000;
	sram_addr_mux		= 2'b00;
	alu_src 		= 2'b00;
	alu_opcode_mux 		= 0;
	load_PC 		= 0;
	load_IR 		= 0;
	rf_data_in_mux  	= 2'b00;

	case (prev_state)
		IDLE: next_state = start ? INIT : IDLE;
		INIT: next_state = start ? INIT : FETCH;

		FETCH: begin
			// Wait for memory to be ready and read instruction
			// Memory is ready, load instruction into IR
			rf_chip_en 	= 1;    
			sram_read_en 	= 1;
			if (sram_mem_ready == 1) begin
				next_state = EXECUTE;
				load_IR 	= 1;
				load_PC 	= 1; // go to next instruction.     
			end else begin
				next_state = FETCH;
			end
		end
		// ===========================================
		// STATE: 	EXECUTE2
		// ===========================================
		EXECUTE1: begin
			next_state 	= FETCH;
			rf_chip_en 	= 1;      
			rf_write_en_n 	= 0;   

			case (ctrl_opcode)
				RTYPE : begin
					// RTYPE instructions read only from register 
					// and use {func3, func7[5]}
					alu_opcode_mux 	= 0;
					alu_src 	= 2'b00;
				end
				ITYPEIMM : begin
					// ITYPE instructions read from imm
					// and use {func3, 0} (except for SLL, SRL, SRA)
					// SLL SRL SRA use func7[5] and immediate shift (shamt = imm[4:0])
					alu_opcode_mux 	= 1;
					alu_src 	= 2'b01;

					if((ctrl_func3 == 3'b001) || (ctrl_func3 == 3'b101)) begin
						alu_opcode_mux 	= 0;
						alu_src 	= 2'b10;
					end
				end
				ITYPELOAD : begin
					// For LOAD/STORE we go to EXECUTE2.
					// There is no need to write to RF right now...
					next_state = EXECUTE2;
					rf_write_en_n = 1;
				end
				UTYPELUI : begin
					// Load RD = {IMM[31:12], 12'd0} directly.
					rf_data_in_mux = 2'b01;
				end
				UTYPEAUIPC : begin
					// Load RD = PC + {IMM[31:12], 12'd0} directly.
					rf_data_in_mux = 2'b10;
				end
				default: begin
					// This case is for invalid opcodes (shutdown).
					// next_state = IDLE;
					rf_write_en_n = 1;
				end
			endcase
            	end
		// ===========================================
		// STATE: 	EXECUTE2
		// ===========================================
		EXECUTE2 : begin
			next_state 	= FETCH;
			rf_chip_en 	= 1;      
			rf_write_en_n 	= 0;   

			case (ctrl_opcode)
				ITYPELOAD : begin
					// On first cycle we set the address and call sram_read_en.
					// When mem_ready is 1 we manipulate input and go to FETCH
					// Otherwise wait for mem_ready and hold address mux open.
					sram_read_en  = 1;
					if(sram_mem_ready == 1'b1) begin
						// ==================================================
						// SRAM_READ_MASK_MUX
						// ==================================================
						// 000		4 BYTES 
						// 001		2 BYTES SIGNEXT
						// 010		1 BYTE	SIGNEXT
						// 011		2 BYTES ZEROEXT
						// 100		1 BYTE	ZEROEXT
						case (ctrl_func3) 
							// LB: [rd] = SIGNEXT(MEM[rs1+imm]) read single byte.
							// LH: [rd] = SIGNEXT(MEM[rs1+imm]) read 2 bytes.
							// LW: [rd] = MEM[rs1+imm] read 4 bytes.
							// LBU : [rd] = ZEROEXT(MEM[rs1+imm]) read single byte.
							// LHU : [rd] = ZEROEXT(MEM[rs1+imm]) read 2 bytes.
							3'b000 : sram_read_mask_mux 	= 3'b010;	
							3'b001 : sram_read_mask_mux 	= 3'b001;
							3'b010 : sram_read_mask_mux	= 3'b000;
							3'b100 : sram_read_mask_mux 	= 3'b100;
							3'b101 : sram_read_mask_mux 	= 3'b011;
						endcase
						rf_data_in_mux = 2'b11; // Read from SRAM into RF.
					end else begin
						next_state = EXECUTE2;
						// ==================================================
						// SRAM_ADDR_MUX
						// ==================================================
						// 00 		PC
						// 01 		4 BYTE WORD
						// 10		2 BYTE HALFWORD
						// 11		1 BYTE
						case (ctrl_func3) 
							// LB: [rd] = SIGNEXT(MEM[rs1+imm]) read single byte.
							// LH: [rd] = SIGNEXT(MEM[rs1+imm]) read 2 bytes.
							// LW: [rd] = MEM[rs1+imm] read 4 bytes.
							// LBU : [rd] = ZEROEXT(MEM[rs1+imm]) read single byte.
							// LHU : [rd] = ZEROEXT(MEM[rs1+imm]) read 2 bytes.
							3'b000 : sram_addr_mux 		= 2'b11;
							3'b001 : sram_addr_mux		= 2'b10;
							3'b010 : sram_addr_mux		= 2'b01;
							3'b100 : sram_addr_mux		= 2'b11;
							3'b101 : sram_addr_mux		= 2'b10;
						endcase
					end
				end
			endcase

		end
		

            default: begin
                next_state = IDLE; 
            end
        endcase
end
endmodule;


/* DATAPATH DP */
module datapath (
	input logic clk,
	input logic rst_n,


	// Interface with Controller 
	input logic rf_chip_en,
	input logic rf_write_en_n,
	input logic sram_read_en,
	input logic sram_write_en,
	input logic [2:0] sram_read_mask_mux,
	input logic [1:0] sram_addr_mux,
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
logic [31:0] imm;	// originally [11:0] but sign extend.
logic [31:0] shamt;	// originally [4:0]  but sign extend.
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

	/* LOGIC FOR R-RTYPE INSTRUCTION */
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
	// SHAMT is used for immediate shift amount (zero extend).
	imm	= {20{IR[31]}, IR[31:20]};
	shamt	= {27'd0, imm[4:0]};
	
	/* REGISTER FILE CONNECTIONS */
	rf_write_addr 	= rd1;
	rf_read_addr_1 	= rs1;
	rf_read_addr_2  = rs2;
	
	// Special cases for UTYPE (LUI) and (AUIPC) that have direct link into rf_data_in.
	// Last case is for Load instructions going from sram directly to rf_data_in.
	case (rf_data_in_mux)
		2'b00 : rf_data_in = alu_out;
		2'b01 : rf_data_in = {IR[31:12], 12'd0};
		2'b10 : rf_data_in = {IR[31:12], 12'd0} + PC;
		2'b11 : rf_data_in = sram_data_out_masked;
	endcase
 
	/* ALU CONNECTIONS */
	// alu_opcode needs to ignore func7[5] for ITYPE instructions, we therefore
	// have a mux with signal from controller based on instruction opcode.
	// alu_src decides to read from register (R) or immediate (I), or shamt (I).
	alu_opcode 	= (alu_opcode_mux) ? {func3, 1'b0} : {func3, func7[5]}; 
	alu_in_a 	= rf_data_out_1;
	case (alu_src)
		2'b00 : alu_in_b = rf_data_out_2;
		2'b01 : alu_in_b = imm;
		2'b10 : alu_in_b = shamt;
	endcase

	/* SRAM CONNECTIONS */

	// sram_data_out is masked for bytes (1b), halfwords (2b) , or words of (4b) (default).
	// Also there are unsigned and signed extensions of bytes and halfwords.
	case (sram_read_mask_mux)
		3'b000 : sram_data_out_masked = sram_data_out;
		3'b001 : sram_data_out_masked = {16{sram_data_out[15]}, sram_data_out[15:0]}; 
		3'b010 : sram_data_out_masked = {24{sram_data_out[7]}, sram_data_out[7:0]}; 
		3'b011 : sram_data_out_masked = {16'd0, sram_data_out[15:0]};
		3'b100 : sram_data_out_masked = {24'd0, sram_data_out[7:0]};
	endcase

	// SRAM_ADDR is either PC or direct value but this value has to be aligned.
	// WORD  4 BYTE : address is aligned to multiple of 4.
	// HWORD 2 BYTE : address is aligned to multiple of 2.
	// BYTE	 1 BYTE : address can be any value.
	case (sram_addr_mux) 
		2'b00 : sram_addr <= PC;
		2'b01 : sram_addr <= ({27'd0, rs1} + imm) & ~(2'b11); 	// 4 byte
		2'b10 : sram_addr <= ({27'd0, rs1} + imm) & ~(1'b1);	// 2 byte
		2'b11 : sram_addr <= ({27'd0, rs1} + imm);		// 1 byte
		default: sram_addr <= PC;
	end

end;

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
		IR <= sram_data_out;	// CHANGE TO SRAM_DATA_OUT_MASKED???
end
endmodule;
