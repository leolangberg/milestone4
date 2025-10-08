`timescale 1ns/1ps

module tb_memory_system;

  // Clock generation (100 MHz)
  logic clk; 
  initial clk = 0; 
  always #5 clk = ~clk;

  // Reset and DUT I/O signals
  logic rst_n;
  logic [31:0] addr;
  logic        read_en, write_en;
  logic [31:0] data_in;
  logic [31:0] data_out;
  logic        mem_ready;

  // Instantiate DUT
  memory_system dut (
    .clk      (clk),
    .rst_n    (rst_n),
    .addr     (addr),
    .read_en  (read_en),
    .write_en (write_en),
    .data_in  (data_in),
    .data_out (data_out),
    .mem_ready(mem_ready)
  );

  // ----------------- Reset Task -----------------
  task automatic reset();
    begin
      rst_n    = 0;
      addr     = 0;
      read_en  = 0;
      write_en = 0;
      data_in  = 0;
      repeat (5) @(posedge clk);
      rst_n = 1; 
      @(posedge clk);
    end
  endtask

  // ----------------- Handshake Write Task -----------------
  // Perform one write transaction: hold addr and data_in stable 
  // while write_en = 1, wait for mem_ready, then deassert.
  task automatic write_byte(input [31:0] a, input [7:0] b);
    begin
      addr     = a;
      data_in  = {4{b}};
      write_en = 1'b1;
      read_en  = 1'b0;

      @(posedge clk);
      wait (mem_ready==1); // Keep addr/data_in stable until mem_ready
      @(posedge clk);
      write_en = 1'b0;
    end
  endtask

  // ----------------- Read Task -----------------
  // Perform one read transaction: hold addr stable 
  // while read_en = 1, wait for mem_ready, then deassert.
  task automatic read_word(input [31:0] a);
    begin
      addr    = a;
      read_en = 1'b1;
      write_en= 1'b0;

      @(posedge clk);
      wait (mem_ready==1);
      @(posedge clk);
      read_en = 1'b0;
    end
  endtask

  // ----------------- Main Stimulus -----------------
  initial begin
    reset();

    // Four byte-wise writes: AA, BB, CC, DD -> 0xDDCCBBAA
    write_byte(32'h0000_0040, 8'hAA);
    write_byte(32'h0000_0041, 8'hBB);
    write_byte(32'h0000_0042, 8'hCC);
    write_byte(32'h0000_0043, 8'hDD);

    // Read back for waveform observation
    read_word(32'h0000_0040);

    #200 $finish;
  end

endmodule
