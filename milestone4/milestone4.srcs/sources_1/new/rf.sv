
// rtl/rf.sv
// Register File (RF) ? IL2234 Milestone 2
// - async active-low reset
// - two async read ports (combinational)
// - one sync write port (posedge clk)
// - chip_en (active-high) gates both reads and writes
// - write_en_n (active-low)

module RF #(
    parameter int BW    = 32,     // data bitwidth
    parameter int DEPTH = 32      // number of rows (registers)
) (
    input  logic                    clk,
    input  logic                    rst_n,        // async active-low
    input  logic                    chip_en,      // active-high
    input  logic                    write_en_n,   // active-low

    input  logic signed [BW-1:0]    data_in,

    input  logic [$clog2(DEPTH)-1:0] read_addr_1,
    input  logic [$clog2(DEPTH)-1:0] read_addr_2,
    input  logic [$clog2(DEPTH)-1:0] write_addr,

    output logic signed [BW-1:0]    data_out_1,
    output logic signed [BW-1:0]    data_out_2
);

    // -----------------------------
    // Storage array
    // -----------------------------
    logic signed [BW-1:0] mem [0:DEPTH-1];

    // -----------------------------
    // Synchronous write + async reset
    // -----------------------------
    int i; // for reset loop (synthesizable)
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            // Reset: mem(all) = 0
            for (i = 0; i < DEPTH; i++) begin
                mem[i] <= '0;
            end
        end else begin
            // Write: on clk rising edge, when enabled
            if (chip_en && !write_en_n) begin
                mem[write_addr] <= data_in;
            end
        end
    end

    // -----------------------------
    // Combinational reads + standby behavior
    // -----------------------------
    always_comb begin
        // Default: outputs 0 (Standby or when chip_en=0)
        data_out_1 = '0;
        data_out_2 = '0;

        // Read: pass-through selected registers when chip enabled
        if (chip_en) begin//chip_en
            data_out_1 = mem[read_addr_1];
            data_out_2 = mem[read_addr_2];
        end
    end

endmodule
