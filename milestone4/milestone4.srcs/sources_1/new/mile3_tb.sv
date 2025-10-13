module memory_system_tb;

logic clk;
logic [31:0] addr;
logic read_en;
logic write_en;
logic [31:0] data_in;
logic [31:0] data_out;
logic mem_ready;

memory_system DUT(
	.clk(clk),
	.rst_n(rst_n),
	.addr(addr),
	.read_en(read_en),
	.write_en(write_en),
	.data_in(data_in),
	.data_out(data_out),
	.mem_ready(mem_ready)
);

initial begin
	clk = 0;
	forever #5 clk = ~clk;
end

initial begin
	rst_n = 0;
	@(posedge clk);

	// IDLE
	rst_n = 1;
	read_en = 1;
	addr = 32'h00000008;
	@(posedge clk);
	
	// LOAD

	@(posedge clk);
	
	// READ

	@(posedge clk);
	
	// DONE
	read_en = 0;
	
	@(posedge clk);:
end:
endmodule;
