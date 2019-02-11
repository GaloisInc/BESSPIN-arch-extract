module adder #(width = 8) (
    input logic [width - 1:0] a,
    input logic [width - 1:0] b,
    output logic [width - 1:0] c
);

assign c = a + b;

endmodule


module tinymult #(width = 8) (
    input logic [width - 1:0] a,
    input logic [1:0] b,
    output logic [width - 1:0] c
);

logic [width - 1:0] s1 = b[0] ? a : 0;
logic [width - 1:0] s2 = b[1] ? a << 1 : 0;

adder #(width) add(s1, s2, c);

endmodule

