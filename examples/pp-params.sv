// Test for interpreting preprocessor defines as parameters

`define WIDTH 8
`define WIDTH2 8

module _test (
    input logic [`WIDTH - 1 : 0] a,
    output logic [`WIDTH - 1 : 0] b
);

assign b = a;

endmodule

module _test2 (
    input logic [`WIDTH - 1 : 0] a,
    output logic [`WIDTH - 1 : 0] b
);
_test t(a, b);
endmodule

module _test3 (
    input logic [7:0] a,
    output logic [7:0] b
);
_test t(a, b);
endmodule

module top (
    input logic [`WIDTH-1:0] a,
    input logic [`WIDTH2-1:0] a2,
    output logic [`WIDTH-1:0] b,
    output logic [`WIDTH2-1:0] b2
);
_test2 t(a, b);
_test3 t2(a2, b2);
endmodule
