/**
 * 	RTYPE INSTRUCTIONS
 * ===================================================
 * instr.	opcode	func3	func7		desc.
 * ===================================================
 * ADD		0110011 000	0000000		[rd] = [rs1] + [rs2] 
 * SUB		0110011 000	0100000		[rd] = [rs1] - [rs2]
 * SLT		0110011 010	0000000		[rd] = [rs1] < [rs2] ? 1 : 0
 * SLTU		0110011 011	0000000		[rd] = [rs1] < [rs2] ? 1 : 0
 * XOR 		0110011 100	0000000		[rd] = [rs1] ^ [rs2]
 * SLL		0110011 001	0000000		[rd] = [rs1] << [rs2]
 * SRL		0110011 101	0000000		[rd] = [rs1] >> [rs2]
 * SRA 		0110011 101	0100000		[rd] = [rs1] >>> [rs2]
 * OR 		0110011 110	0000000		[rd] = [rs1] | [rs2]
 * AND		0110011 111	0000000		[rd] = [rs1] & [rs2]
 */
module ALU #( BW = 32 ) (
	input  logic signed [BW-1:0] in_a,
	input  logic signed [BW-1:0] in_b,
	input  alu_op       	     opcode, 
	output logic signed [BW-1:0] out,
	output logic        [2:0] flags 
);
	// alu_op means [3:0]
	// {overflow, negative, zero}
	// opcode: {func3, func7[5]}
	logic v, n, z, c;

	typedef enum logic [3:0] {
		ADD 	= 4'b0000,
		SUB 	= 4'b0001,
		SLT 	= 4'b0100,
		SLTU 	= 4'b0110,
		XOR 	= 4'b1000,
		SLL 	= 4'b0010,
		SRL	= 4'b1010,
		SRA	= 4'b1011,
		OR	= 4'b1100,
		AND	= 4'b1110,
		
	} alu_op;

	always_comb begin
		case(opcode)
			ADD 	: {c, out} = in_a + in_b;
			SUB 	: {c, out} = in_a - in_b;
			SLT 	: out = (in_a < in_b) ? 1 : 0;
			SLTU 	: out = ($unsigned(in_a) < $unsigned(in_b)) ? 1 : 0;
			XOR 	: out = in_a ^ in_b;
			SLL 	: out = in_a << in_b;
			SRL 	: out = in_a >> in_b;
			SRA	: out = in_a >>> in_b;
			OR	: out = in_a | in_b;
			AND	: out = in_a & in_b;
		endcase	
		
		v = 1'b0;
		if (opcode == ADD) begin
			// 2 positive numbers give negative sign or
			// 2 negative numbers give positive sign
			if((in_a[BW-1] & in_b[BW-1] & ~w[BW-1]) | 
			  (~in_a[BW-1] & ~in_b[BW-1] & w[BW-1]))
				v = 1'b1;
		end
		if(opcode == SUB) begin
			// same as addition but in subtraction in_b is inversed. (A + (-B))
			if((in_a[BW-1] & ~in_b[BW-1] & ~w[BW-1]) | 
			  (~in_a[BW-1] & in_b[BW-1] & w[BW-1]))
				v = 1'b1;
		end

	end

	assign n = out[BW-1];
	assign z = (out == 0) ? 1 : 0;	// ~|out
	
	assign flags = {v,n,z};
	
e
