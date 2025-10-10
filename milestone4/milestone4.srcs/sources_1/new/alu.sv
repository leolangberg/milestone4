module ALU #(
    parameter BW = 32
)(
    input  logic signed [BW-1:0] in_a,
    input  logic signed [BW-1:0] in_b,
    input  logic [3:0]           opcode,
    output logic signed [BW-1:0] out,
    output logic [2:0]           flags
);

    logic overflow, negative, zero;
    logic signed [BW:0] tmp;  // caculation outcome

    always_comb begin
        overflow = 0;
        case (opcode)
            4'b0000: begin // ADD
                tmp = in_a + in_b;
                out = tmp[BW-1:0];
                overflow = (in_a[BW-1] == in_b[BW-1]) && (out[BW-1] != in_a[BW-1]);
            end
            4'b0001: begin // SUB
                tmp = in_a - in_b;
                out = tmp[BW-1:0];
                overflow = (in_a[BW-1] != in_b[BW-1]) && (out[BW-1] != in_a[BW-1]);
            end
            4'b0010: out = in_a & in_b; // AND
            4'b0011: out = in_a | in_b; // OR
            4'b0100: out = in_a ^ in_b; // XOR
            4'b0101: out = in_a + 1;    // INC
            4'b0110: out = in_a;        // MOVA
            4'b0111: out = in_b;        // MOVB
            default: out = '0;
        endcase

        negative = (out < 0);
        zero     = (out == 0);
    end

    assign flags = {overflow, negative, zero};

endmodule
