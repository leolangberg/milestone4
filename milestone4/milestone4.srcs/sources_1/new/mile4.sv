/* TOP MODULE */
module top (
	input logic clk,
	input logic rst_n,
	input logic start,
	output logic read_en,
	output logic write_en,
	output logic mem_ready,
	output logic [31:0] addr,
	output logic [31:0] data_in,
	output logic [31:0] data_out
	
);

logic rf_chip_en;
logic rf_write_en_n;
logic sram_read_en;
logic sram_write_en;
logic [2:0] sram_read_mask_mux;
logic [1:0] sram_addr_mux;
logic [1:0] sram_addr_imm_mux;
logic [1:0] sram_addr_write_mux;
logic sram_mem_ready;
logic load_IR;
logic load_PC;
logic [1:0] alu_src;
logic [2:0] alu_opcode_mux;
logic [6:0] ctrl_opcode;
logic [2:0] ctrl_func3;
logic [2:0] rf_data_in_mux;
logic [2:0] branch_logic_mux;
logic branch;
logic jump_UJTYPE;
logic jump_ITYPE;

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
	.sram_addr_imm_mux(sram_addr_imm_mux),
	.sram_addr_write_mux(sram_addr_write_mux),
	.sram_mem_ready(sram_mem_ready),
	.ctrl_opcode(ctrl_opcode),
	.ctrl_func3(ctrl_func3),
	.load_IR(load_IR),
	.load_PC(load_PC),
	.alu_opcode_mux(alu_opcode_mux),
	.alu_src(alu_src),
	.rf_data_in_mux(rf_data_in_mux),
	.branch_logic_mux(branch_logic_mux),
	.branch(branch),
	.jump_UJTYPE(jump_UJTYPE),
	.jump_ITYPE(jump_ITYPE)
	
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
	.sram_addr_imm_mux(sram_addr_imm_mux),
	.sram_addr_write_mux(sram_addr_write_mux),
	.sram_mem_ready(sram_mem_ready),
	.ctrl_opcode(ctrl_opcode),
	.ctrl_func3(ctrl_func3),
	.load_IR(load_IR),
	.load_PC(load_PC),
	.alu_opcode_mux(alu_opcode_mux),
	.alu_src(alu_src),
	.rf_data_in_mux(rf_data_in_mux),
	.branch_logic_mux(branch_logic_mux),
	.branch(branch),
	.jump_UJTYPE(jump_UJTYPE),
	.jump_ITYPE(jump_ITYPE),
	.addr(addr),
	.data_in(data_in),
	.data_out(data_out)
	
);

assign read_en = sram_read_en;
assign write_en = sram_write_en;
assign mem_ready = sram_mem_ready;
	
endmodule;

typedef enum logic [6:0] { 
	RTYPE 		= 7'b0110011, 
	ITYPEIMM	= 7'b0010011,
	ITYPELOAD	= 7'b0000011,
	UTYPELUI	= 7'b0110111,
	UTYPEAUIPC	= 7'b0010111,
	STYPE		= 7'b0100011,
	SBTYPE		= 7'b1100011,
	UJTYPE      = 7'b1101111,
	ITYPEJUMP   = 7'b1100111
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
	output logic [1:0] sram_addr_imm_mux,
	output logic [1:0] sram_addr_write_mux,
	input logic sram_mem_ready,
	input instruction_opcode ctrl_opcode,	// [6:0] opcode.
	input logic [2:0] ctrl_func3,
	output logic load_IR,
	output logic load_PC,
	output logic [1:0] alu_src,
	output logic [2:0] alu_opcode_mux,
	output logic [2:0] rf_data_in_mux,
	output logic [2:0] branch_logic_mux,
	output logic branch,
	output logic jump_UJTYPE,
	output logic jump_ITYPE

);


typedef enum logic [2:0] { IDLE, INIT,  FETCH, EXECUTE1, EXECUTE2 } state;
state prev_state, next_state;

always_ff @(posedge clk or negedge rst_n) begin
    if(!rst_n) prev_state <= IDLE;
    else prev_state <= next_state;
end

always_comb begin
	// Initialization
	rf_chip_en 		= 0;
	rf_write_en_n 		= 1;
	sram_read_en 		= 0;
	sram_write_en 		= 0;
	sram_read_mask_mux 	= 3'b000;
	sram_addr_mux		= 2'b00;
	sram_addr_imm_mux	= 2'b00;
	sram_addr_write_mux	= 2'b00;
	alu_src 		     = 2'b00;
	alu_opcode_mux 		= 3'b000;
	load_PC 		= 0;
	load_IR 		= 0;
	rf_data_in_mux  	= 3'b000;
	branch_logic_mux = 3'b000;
	branch = 1'b0;
	jump_UJTYPE = 1'b0;
	jump_ITYPE = 1'b0;

	case (prev_state)
		IDLE: next_state = start ? INIT : IDLE;
		INIT: next_state = start ? INIT : FETCH;

		FETCH: begin
			// Wait for memory to be ready and read instruction
			// Memory is ready, load instruction into IR
			rf_chip_en 	= 1;    
			rf_write_en_n	= 1;
			if (sram_mem_ready == 1) begin
				next_state = EXECUTE1;
				load_IR 	   = 1;
				
			end else begin
				next_state = FETCH;
				sram_read_en 	= 1;
				
			end
		end
		// ===========================================
		// STATE: 	EXECUTE1
		// ===========================================
		EXECUTE1: begin
			next_state 	= FETCH;
			rf_chip_en 	= 1;      
			rf_write_en_n 	= 1;   
			load_PC 	= 1; // go to next instruction.     

			case (ctrl_opcode)
				RTYPE : begin
					// RTYPE instructions read only from register 
					// and use {func3, func7[5]}
					alu_opcode_mux 	= 3'b000;
					alu_src 	= 2'b00;
					rf_write_en_n = 0;
				end
				ITYPEIMM : begin
					// ITYPE instructions read from imm
					// and use {func3, 0} (except for SLL, SRL, SRA)
					// SLL SRL SRA use func7[5] and immediate shift (shamt = imm[4:0])
					alu_opcode_mux 	= 3'b001;
					alu_src 	= 2'b01;
					rf_write_en_n = 0;

					if((ctrl_func3 == 3'b001) || (ctrl_func3 == 3'b101)) begin
						alu_opcode_mux 	= 3'b000;
						alu_src 	= 2'b10;
					end
				end
				ITYPELOAD : begin
					// For LOAD/STORE we go to EXECUTE2.
					// There is no need to write to RF right now...
					next_state = EXECUTE2;
					rf_write_en_n = 1;
				end
				ITYPEJUMP : begin
				    // JUMP INSTRUCTION JALr
				    // [rd] = PC + 4, PC = ([RS1] + IMM)
				    
				    // RF in is PC + 4;
				    rf_data_in_mux = 3'b011;
				    rf_write_en_n = 0;
				    jump_ITYPE = 1;
				    
				    // Choose address is from immediate value (not PC)
					// immediate value is from ITYPE.
					sram_addr_mux = 2'b01;
					sram_addr_imm_mux = 2'b00;
			
				end
				UTYPELUI : begin
					// Load RD = {IMM[31:12], 12'd0} directly.
					rf_data_in_mux = 2'b001;
					rf_write_en_n = 0;
				end
				UTYPEAUIPC : begin
					// Load RD = PC + {IMM[31:12], 12'd0} directly.
					rf_data_in_mux = 2'b010;
					rf_write_en_n = 0;
				end
				STYPE : begin
					// STORE INSTRUCTION
					// MEM[rs1 + imm] = [rs2]
					next_state = EXECUTE2;
					rf_write_en_n = 1;
					
				end
				SBTYPE : begin
					// BRANCH INSTRUCTIONS
					// Reroute alu_opcode to perform comparison or sub.
					// ==========================================
					// BEQ	rs1 == rs2 	z=1 SUB	 000	0001
					// BNE  rs1 != rs2	z=0 SUB	 001	0001
					// BLT	rs1 < rs2 sign      SLT	 100	0100
					// BGE  rs1 >= rs2 sign    ~SLT  101	0100
					// BLTU rs1 < rs2 uns	    SLTU 110	0110
					// BGEU rs1 >= rs2 uns	   ~SLTU 111	0110
					rf_write_en_n = 1;
					case (ctrl_opcode)
						3'b000 : alu_opcode_mux = 3'b010;
						3'b001 : alu_opcode_mux = 3'b010;
						3'b100 : alu_opcode_mux = 3'b011;
						3'b101 : alu_opcode_mux = 3'b011;
						3'b110 : alu_opcode_mux = 3'b100;								  
						3'b111 : alu_opcode_mux = 3'b100;
					endcase
					// Logic for branch checking has same mux code as func3.
					// 000, 001, 100, 101, 110, 111
					branch_logic_mux = ctrl_func3;
					branch = 1;
					
					// Choose address is from immediate value (not PC)
					// immediate value is from STYPE.
					sram_addr_mux = 2'b01;
					sram_addr_imm_mux = 2'b10;
					
				end
				UJTYPE : begin
				    // UNCONDITIONAL JUMP INSTRUCTION JAL
				    // [rd] = PC + 4, PC = PC + IMM
				    
				    // RF in is PC + 4;
				    rf_data_in_mux = 3'b011;
				    rf_write_en_n = 0;
				    jump_UJTYPE = 1;
				    
				    // Choose address is from immediate value (not PC)
				    // immediate value is from UJTYPE
				    sram_addr_mux = 2'b01;
				    sram_addr_imm_mux = 2'b11;
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
			rf_write_en_n 	= 1;   

			case (ctrl_opcode)
				ITYPELOAD : begin
					// On first cycle we set the address and call sram_read_en.
					// When mem_ready is 1 we manipulate input and WRITE then go FETCH.
					// Otherwise wait for mem_ready and hold address mux open.
					if(sram_mem_ready == 1'b1) begin
						
						next_state = FETCH;
						rf_data_in_mux = 2'b100; // Read from SRAM into RF.
                          			rf_write_en_n 	= 0;
						
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
					end else begin
						next_state = EXECUTE2;
						sram_read_en  	  = 1;
						sram_addr_imm_mux = 2'b00; // use imm[31:0]
						sram_addr_mux = 2'b01;	   // addr is decided directly not by PC...
					end
				end

				STYPE : begin
					// For Store operations we set the address according to byte size
					// Then we just wait for mem_ready...
					if(sram_mem_ready == 1) begin
						next_state = FETCH;
					end else begin
						next_state = EXECUTE2;
						sram_write_en = 1;
						sram_addr_imm_mux = 2'b01; // use imm[31:25] imm[11:7]
						sram_addr_mux = 2'b01;	// addr is decided directly not by PC...
						// ==================================================
						// SRAM_ADDR_WRITE_MUX (needs to separate because not same IMM.
					   // NOTE: This is just for the mask, we use BOTH write_en (1b) and wea_mask(4b)
					   // SO FOR WRITE YOU HAVE TO HAVE BOTH WRITE_MASK AND WRITE_EN
						// ==================================================
						// 01 		1 BYTE 
						// 10		2 BYTE HALFWORD
						// 11		4 BYTE WORD
						case (ctrl_func3) 
							3'b000 : sram_addr_write_mux 	= 2'b01;
							3'b001 : sram_addr_write_mux 	= 2'b10;
							3'b010 : sram_addr_write_mux	= 2'b11;
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
	output logic [31:0] addr,
	output logic [31:0] data_in,
	output logic [31:0] data_out,


	// Interface with Controller 
	input logic rf_chip_en,
	input logic rf_write_en_n,
	input logic [2:0] rf_data_in_mux,
	input logic sram_read_en,
	input logic sram_write_en,
	input logic [2:0] sram_read_mask_mux,
	input logic [1:0] sram_addr_mux,
	input logic [1:0] sram_addr_imm_mux,
	input logic [1:0] sram_addr_write_mux,
	output logic sram_mem_ready,
	output logic [6:0] ctrl_opcode,
	output logic [2:0] ctrl_func3,
	input logic load_IR,
	input logic load_PC,
	input logic alu_src,
	input logic [2:0] alu_opcode_mux,
	input logic [2:0] branch_logic_mux,
	input logic branch,
	input logic jump_UJTYPE,
	input logic jump_ITYPE
	
);


logic signed [31:0] PC; // PROGRAM COUNTER 
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
logic signed [31:0] sram_addr_imm;
logic branch_logic;
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
	.data_in(rf_data_in), // sram_data_out
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
logic [31:0] 	sram_addr;
logic [1:0]     sram_addr_low2bytes;
logic [3:0] 	sram_write_mask;
logic [31:0] 	sram_data_in;
logic [31:0] 	sram_data_out;
logic [31:0]	sram_data_out_masked;

memory_system sram(
	.clk(clk),
	.rst_n(rst_n),
	.addr(sram_addr),
	.read_en(sram_read_en),
	.write_en(sram_write_mask),
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
	imm	= {{20{IR[31]}}, IR[31:20]};
	shamt	= {27'd0, imm[4:0]};

	/* LOGIC FOR STYPE INSTRUCTION */
	
	/* REGISTER FILE CONNECTIONS */
	rf_write_addr 	= rd1;
	rf_read_addr_1 	= rs1;
	rf_read_addr_2  = rs2;
	
	// =========================================================================
	// =   Muxes that work together have to be nested otherwise bugs occur...  =
	// =========================================================================
	
	/* RF_DATA_IN_MUX */
	// =========================================================================
	//  000    RF_in comes from ALU
	//  001    special UTYPE (LUI)
	//  010    special UTYPE (AUIPC)
	//  011    special UJTYPE (JAL)
	//  100    RF_in comes from SRAM out
	case (rf_data_in_mux)
		3'b000 : rf_data_in = alu_out;
		3'b001 : rf_data_in = {IR[31:12], 12'd0};
		3'b010 : rf_data_in = {IR[31:12], 12'd0} + PC;
		3'b011 : rf_data_in = (PC + 4);
		3'b100 : begin
		        /* SRAM_READ_MASK_MUX */
                // =========================================================================
                //  000     LW  4 bytes 1 choice     (mask = out)
                //  001     LH  2 bytes 2 choices   (mask is upper or lower signed)  
                //  010     LB  1 byte  4 choices    (signed)
                //  011     LHU 2 bytes 2 choices       (unsigned)
                //  100     LBU 1 byte  4 choices       (unsigned)
                case (sram_read_mask_mux)
                    // LW loads entire word without any changes.
                    3'b000 : sram_data_out_masked = sram_data_out;
                    3'b001 : begin
                        // LH loads signed halfword based on addr[1]
                        case (sram_addr_low2bytes)
                                2'b00 : sram_data_out_masked = {{16{sram_data_out[15]}}, sram_data_out[15:0]};
                                2'b10 : sram_data_out_masked = {sram_data_out[31:16], 16'd0};
                            endcase
                    end
                    3'b010 : begin
                            // LB load signed byte based on addr[1:0]
                            case (sram_addr_low2bytes)
                                2'b00 : sram_data_out_masked = {{24{sram_data_out[7]}}, sram_data_out[7:0]};
                                2'b01 : sram_data_out_masked = {{16{sram_data_out[15]}}, sram_data_out[15:8], 8'd0};
                                2'b10 : sram_data_out_masked = {{8{sram_data_out[23]}}, sram_data_out[23:16], 16'd0};
                                2'b11 : sram_data_out_masked = {sram_data_out[31:24], 24'd0};   
                            endcase
                    end
                    3'b011 : begin 
                            // LHU unsigned upper or lower halfword based on addr[1] signal
                            case (sram_addr_low2bytes)
                                2'b00 : sram_data_out_masked = {{16'd0}, sram_data_out[15:0]};
                                2'b10 : sram_data_out_masked = {sram_data_out[31:16], 16'd0};
                            endcase
                    end 
                    3'b100 : begin
                          // LBU unsigned byte based on addr[1:0]
                         case (sram_addr_low2bytes)
                                2'b00 : sram_data_out_masked = {24'd0, sram_data_out[7:0]};
                                2'b01 : sram_data_out_masked = {16'd0, sram_data_out[15:8], 8'd0};
                                2'b10 : sram_data_out_masked = {8'd0, sram_data_out[23:16], 16'd0};
                                2'b11 : sram_data_out_masked = {sram_data_out[31:24], 24'd0};   
                            endcase
                   end
                   default: sram_data_out_masked = sram_data_out;
                endcase 
		      rf_data_in = sram_data_out_masked;
		end	
	endcase
 
	/* ALU CONNECTIONS */
	// ALU_OPCODE needs to ignore func7[5] for ITYPE instructions, we therefore
	// have a mux with signal from controller based on instruction opcode.
	// There is also an fabricated SUB SLT SLTU for BRANCH comparison operations.
	case (alu_opcode_mux) 
		3'b000 : alu_opcode = {func3, func7[5]};
		3'b001 : alu_opcode = {func3, 1'b0};
		3'b010 : alu_opcode = 4'b0001;	// SUB
		3'b011 : alu_opcode = 4'b0100;	// SLT
		3'b100 : alu_opcode = 4'b0110;	// SLTU

	endcase
	// alu_src decides to read from register (R) or immediate (I), or shamt (I).
	case (alu_src)
		2'b00 : alu_in_b = rf_data_out_2;
		2'b01 : alu_in_b = imm;
		2'b10 : alu_in_b = shamt;
	endcase
	alu_in_a = rf_data_out_1;

	/* SRAM CONNECTIONS */
	
	// Write data is specified by inside of register 2 [rs2].
	sram_data_in = rf_data_out_2;
	sram_addr_low2bytes = sram_addr[1:0];
	
	// SRAM_ADDR is either PC or direct value but this value has to be aligned.
	// THERE IS NO NEED TO ALIGN HERE BECAUSE THIS MULTIPLE 4 ALIGNMENT IS HANDLE
	// AUTOMATICALLY BY 32-BIT BYTE ADDRESSABLE MEMORY.
	    // WORD  4 BYTE : address is aligned to multiple of 4.
	    // HWORD 2 BYTE : address is aligned to multiple of 2.
	    // BYTE	 1 BYTE : address can be any value.
	    // SRAM_ADDR_IMM Decides how to construct immediate value used 
	    // as part of address calculation.
	// 00 for ITYPE IMM[31:0] 
	// 01 for STYPE IMM[31:25] IMM[11:7]
	// 10 for SBTYPE IMM[31] IMM[7] IMM[30:25] IMM[11:8] IMM HERE IS SIGNED SO IR[31] is sign...
	// 11 for UJTYPE IMM[20] IMM[10:1] IMM[11], IMM [19:12]
	case (sram_addr_mux) 
                2'b00 : sram_addr = PC;
                2'b01 : begin
                    case (sram_addr_imm_mux)
                        2'b00 : sram_addr_imm = imm;
                        2'b01 : sram_addr_imm = {20'd0, IR[31:25], IR[11:7]};	// STYPE IMM
			            2'b10 : sram_addr_imm = {{19{IR[31]}}, IR[31], IR[7], IR[30:25], IR[11:8], 1'b0}; // SBTYPE IMM SIGNED
			            2'b11 : sram_addr_imm = {{11{IR[20]}}, IR[19:12], IR[20], IR[31:21], 1'b0};  // UJTYPE IMM SIGNED
                    endcase
                    sram_addr = (rf_data_out_1 + sram_addr_imm); 
                end
                default: sram_addr = PC;
        endcase


	// will set WEA to correct values depending on if bytes, halfs or words
	// was specified. Should only be called when we are storing.
	// NOTE: This is just for the mask, we use BOTH write_en (1b) and wea_mask(4b)
	// SO FOR WRITE YOU HAVE TO HAVE BOTH WRITE_MASK AND WRITE_EN
	case (sram_addr_write_mux)
		2'b00 : sram_write_mask = 4'b0000;	// default case...
		// SB, write mask is based on addr[1:0]
		2'b01 : begin
			case(sram_addr[1:0])
				2'b00 : sram_write_mask = 4'b0001;
				2'b01 : sram_write_mask = 4'b0010;
				2'b10 : sram_write_mask = 4'b0100;
				2'b11 : sram_write_mask = 4'b1000;
			endcase
	    end
		// SH store halfword upper or lower based on addr[1].
		2'b10 : begin
			case(sram_addr[1:0])
				2'b00 : sram_write_mask = 4'b0011;
				2'b10 : sram_write_mask = 4'b1100;
			endcase
		end
		// SW entire word unchanged.
		2'b11 : sram_write_mask = 4'b1111;
		default: sram_write_mask = 4'b0000;
	endcase;
	
	/* BRANCH LOGIC */
	// ==========================================
	// BEQ	rs1 == rs2 	z=1 SUB	 000	0001
	// BNE  rs1 != rs2	z=0 SUB	 001	0001
	// BLT	rs1 < rs2 sign      SLT	 100	0100
	// BGE  rs1 >= rs2 sign    ~SLT  101	0100
	// BLTU rs1 < rs2 uns	    SLTU 110	0110
	// BGEU rs1 >= rs2 uns	   ~SLTU 111	0110
	case (branch_logic_mux) 
		3'b000 : branch_logic = alu_flags[0]; // z-flag
		3'b001 : branch_logic = ~(alu_flags[0]); // ~z
		3'b100 : branch_logic = alu_out[0];	// SLT 1bit true/false
		3'b101 : branch_logic = ~(alu_out[0]);  // ~SLT
		3'b110 : branch_logic = alu_out[0];	// SLTU 1bit true/false
		3'b111 : branch_logic = ~(alu_out[0]);  // ~SLTU
		default: branch_logic = 1'b0;
	endcase
	
end;

/* PROGRAM COUNTER */
// ============================================================
// Holds Instruction pointer Register (IR)
// Updates PC = PC + 4 (increases addr).
// if flag BRANCH is active AND BRANCH LOGIC == TRUE then we new PC += IMM
// JUMP_ITYPE is for JALR instruction. SRAM_ADDR = [rs1] + IMM already.
always_ff @(posedge clk or negedge rst_n) begin
	if(!rst_n) begin
		PC <= '0;
	end else if (load_PC) begin
		if ((branch & branch_logic) | jump_UJTYPE) begin
			PC <= PC + sram_addr_imm;    
		end else if (jump_ITYPE) begin
		    PC <= (sram_addr & 32'hFFFFFFFE);
		end else begin
			PC <= PC + 4;
		end
	end
end

/* INSTRUCTION REGISTER (IR) */
always_ff @(posedge clk or negedge rst_n) begin
	if(!rst_n) 
		IR <= '0;
	else if (load_IR)
		IR <= sram_data_out;	// CHANGE TO SRAM_DATA_OUT_MASKED???
end

assign addr = sram_addr;
assign data_in = sram_data_in;
assign data_out = sram_data_out;
endmodule;
