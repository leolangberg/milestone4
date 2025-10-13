
module memory_system (
	input  logic        clk,
	input  logic        rst_n,

	// CPU side (byte-addressed)
	input  logic [31:0] addr,       // byte address; word index = addr[15:2]
	input  logic        read_en,    // hold high until mem_ready
	input  logic        write_en,   // hold high until mem_ready
	input  logic [31:0] data_in,
	output logic [31:0] data_out,
	output logic        mem_ready
);

wire [13:0] addra_w = addr[15:2];    // 64 KiB / 4B = 16K words -> 14-bit
  
typedef enum logic [1:0] {IDLE, LOAD, READ, DONE} state;
state prev_state, next_state;
always_ff @(posedge clk or negedge rst_n) begin
	if (!rst_n) prev_state <= IDLE;
	else        prev_state <= next_state;
end


always_comb begin
	next_state = IDLE;
	case (prev_state)
		IDLE : begin
			if(read_en == 1) begin
				next_state = LOAD;
			end else begin
				next_state = IDLE;
			end
		end
		LOAD : begin
			next_state = READ;
		end
		READ : begin
			// We issued read and so memory should be 
			// read on next cycle
			next_state = DONE;
			ip_en = 1;
		end
		DONE : begin
			next_state = IDLE;
			mem_ready = 1'b1;
		end
	endcase
end

logic        ip_ena;
logic [13:0] ip_addra;
logic [31:0] ip_douta;
logic [31:0] ip_dina;
logic [3:0] ip_wea;
assign data_out = ip_douta;
assign ip_addra = addra_w;

sram u_sram (
    .clka  (clk),
    .ena   (ip_ena),
    .wea   (ip_wea),         // 4 bits
    .addra (ip_addra),
    .dina  (ip_dina),
    .douta (ip_douta)
  );
endmodule;
