module mem_sys (
    input  logic        clk,
    input  logic        rst_n,

    // CPU side (byte-addressed)
    input  logic [31:0] addr,       // byte address
    input  logic        read_en,    // hold high until mem_ready
    input  logic   write_en,   // byte enable per lane
    input  logic [31:0] data_in,
    output logic [31:0] data_out,
    output logic        mem_ready
);

   
    logic en, ren, wen;
    logic rst_busy;
    
    logic [3:0] weamask;
    assign weamask = {4{write_en}};

   
    assign wen = !rst_busy && (|weamask);
    assign ren = !rst_busy && read_en;
    assign en  = wen | ren;

    // WE NEED RSTA_BUSY FOR 32BIT BYTE ADDRESSABLE MEM.
    sram u_sram (
        .clka      (clk),
        .rsta      (!rst_n),
        .ena       (en),
        .wea       (weamask),     // 4 bits = byte enables
        .addra     (addr),         // full byte address
        .dina      (data_in),
        .douta     (data_out),
        .rsta_busy  (rst_busy)
    );

    // Simple FSM for memory handshake
    typedef enum logic [1:0] {IDLE, WAIT, DONE} state_t;
    state_t pstate, nstate;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n)
            pstate <= IDLE;
        else
            pstate <= nstate;
    end

    always_comb begin
        nstate = pstate;
        unique case (pstate)
            IDLE: begin
                if (ren)
                    nstate = WAIT; // read takes one extra cycle
                else if (wen)
                    nstate = DONE; // write completes immediately
            end

            WAIT: nstate = DONE; // after read delay
            DONE: nstate = IDLE;
        endcase
    end

    assign mem_ready = (pstate == DONE);

endmodule
