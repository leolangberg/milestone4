`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 14.10.2025 01:29:41
// Design Name: 
// Module Name: mem_sys
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


module mem_sys(
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

wire [13:0] addra_w = addr[15:2]; 
logic en;
logic [3:0] wea_mask;

sram u_sram (
    .clka  (clk),
    .ena   (en),
    .wea   (wea_mask),         // 4 bits
    .addra (addra_w),
    .dina  (data_in),
    .douta (data_out)
  );

typedef enum logic [1:0] {IDLE, WAIT, DONE} state_t;
  state_t pstate, nstate;
  
 always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) pstate <= IDLE;
    else        pstate <= nstate;
  end
  
  always_comb begin 
    en = read_en | write_en;
    wea_mask = 4'b0000;
    case (pstate) 
        IDLE: if(read_en) begin
                    nstate = WAIT;
              end
              else if(write_en) begin
                    wea_mask = 4'b1111;
                    nstate = DONE;
              end else
                    nstate = IDLE;
        WAIT: nstate = DONE;
        DONE: nstate = IDLE;
        default: nstate = IDLE;
    endcase
  end

assign mem_ready = (pstate == DONE);



endmodule
