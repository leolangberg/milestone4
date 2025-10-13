// 64 KiB SRAM wrapper with byte-wise writes (wea[3:0])
// Data width = 32, Depth = 16384, Output registered in IP (1-cycle read latency)

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

  // ---------- Addressing ----------
  wire [13:0] addra_w = addr[15:2];    // 64 KiB / 4B = 16K words -> 14-bit

  // Byte select derived from addr[1:0]
/*
  logic [3:0] wea_mask;
  always_comb begin
    unique case (addr[1:0])
      2'b00: wea_mask = 4'b0001; // lowest byte
      2'b01: wea_mask = 4'b0010;
      2'b10: wea_mask = 4'b0100;
      2'b11: wea_mask = 4'b1000; // highest byte
    endcase
  end
*/
  // ---------- FSM ----------
  typedef enum logic [1:0] {IDLE, READ, WRITE, DONE} state_t;
  state_t state, nstate;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) state <= IDLE;
    else        state <= nstate;
  end

  always_comb begin
    nstate = state;
    unique case (state)
      IDLE : if (read_en) nstate = READ;
             else if (write_en) nstate = WRITE;
      READ : nstate = DONE;   // 1-cycle latency because IP has output register
      WRITE: nstate = DONE;   // write completes this cycle
      DONE : nstate = IDLE;
    endcase
  end

  // ---------- Drive IP ----------
  logic        ip_ena;
  logic [3:0]  ip_wea;
  logic [13:0] ip_addra;
  logic [31:0] ip_dina, ip_douta;

  assign data_out  = ip_douta;
  assign mem_ready = (state == DONE);

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      ip_ena   <= 1'b0;
      ip_wea   <= 4'b0000;
      ip_addra <= '0;
      ip_dina  <= '0;
    end else begin
      unique case (nstate) // look-ahead to hold enables cleanly
        IDLE: begin
          ip_ena <= 1'b0; 
	  ip_wea <= 4'b0000;
        end
        READ: begin
          ip_ena   <= 1'b1;
          ip_wea   <= 4'b0000;
          ip_addra <= addra_w;
        end
        WRITE: begin
          ip_ena   <= 1'b1;
          ip_wea   <= 4'b1111;      // byte-wise write enable
          ip_addra <= addra_w;
          ip_dina  <= data_in;
        end
        DONE: begin
          ip_ena <= 1'b0; 
	  ip_wea <= 4'b0000;
        end
      endcase
    end
  end

  // ---------- SRAM IP (change instance name/ports if needed) ----------
  sram u_sram (
    .clka  (clk),
    .ena   (ip_ena),
    .wea   (ip_wea),         // 4 bits
    .addra (ip_addra),
    .dina  (ip_dina),
    .douta (ip_douta)
  );

endmodule;
