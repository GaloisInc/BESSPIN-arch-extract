module adder (
    input logic [7:0] a,
    input logic [7:0] b,
    output logic [7:0] c
);

assign c = a + b;

endmodule


module tinymult (
    input logic [7:0] a,
    input logic [1:0] b,
    output logic [7:0] c
);

logic [7:0] s1 = b[0] ? a : 0;
logic [7:0] s2 = b[1] ? a << 1 : 0;

adder add(s1, s2, c);

endmodule

