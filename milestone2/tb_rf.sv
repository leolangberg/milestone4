`timescale 1ns/1ps

module tb_rf;

  // ==== ?? ====
  localparam int BW    = 8;
  localparam int DEPTH = 256;
  localparam int AW    = $clog2(DEPTH);

  // ==== ?? ====
  logic                     clk;
  logic                     rst_n;
  logic                     chip_en;
  logic                     write_en_n;     // ??????
  logic signed [BW-1:0]     data_in;
  logic [AW-1:0]            read_addr_1, read_addr_2, write_addr;
  logic signed [BW-1:0]     data_out_1, data_out_2;

  // ==== DUT ====
  rf #(.BW(BW), .DEPTH(DEPTH)) dut (
    .clk, .rst_n, .chip_en, .write_en_n,
    .data_in, .data_out_1, .data_out_2,
    .read_addr_1, .read_addr_2, .write_addr
  );

  // ==== ?? ====
  initial clk = 0;
  always #5 clk = ~clk;  // 100MHz

  // ==== ??? ====
  initial begin
    // ??
    rst_n = 0;
    chip_en = 0;
    write_en_n = 1;
    data_in = '0;
    write_addr = '0;
    read_addr_1 = '0;
    read_addr_2 = '0;

    // ??
    repeat (3) @(posedge clk);
    rst_n = 1;
    @(posedge clk);

    chip_en = 1;

    // --- ???? ---
    // ? addr=0x3A, din=0x77
    write_addr  = 'h3A;
    data_in     = 8'h77;
    write_en_n  = 0;              // ???
    @(posedge clk);               // ??????????
    write_en_n  = 1;

    // ?? addr=0x00, din=0x21
    write_addr  = 'h00;
    data_in     = 8'h21;
    write_en_n  = 0;
    @(posedge clk);
    write_en_n  = 1;

    // --- ???? ---
    // ??1? 0x3A???2? 0x00
    read_addr_1 = 'h3A;
    read_addr_2 = 'h00;
    @(posedge clk);   // ???????
    #1;               // ???????????????
    $display("t=%0t  A1=%h -> D1=%h | A2=%h -> D2=%h",
             $time, read_addr_1, data_out_1, read_addr_2, data_out_2);

    // ????????
    read_addr_1 = 'h00;
    read_addr_2 = 'h3A;
    @(posedge clk);
    #1;
    $display("t=%0t  A1=%h -> D1=%h | A2=%h -> D2=%h",
             $time, read_addr_1, data_out_1, read_addr_2, data_out_2);

    // ??
    #20;
    $finish;
  end

endmodule

