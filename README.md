# Building

Open a Nix shell with all the dependencies:

    nix-shell --pure nix

Compile the (System)Verilog exporter and the architecture extraction tool:

    make


# Running

Running the architecture extraction tool involves two steps.  As with building,
these should be run inside a Nix shell.

First, run the `exporter` tool, which parses (System)Verilog code and dumps an
AST to a file in CBOR format:

    ./exporter examples/tinymult.sv -o tinymult.cbor

This produces `tinymult.cbor`, containing a representation of the AST of
`examples/tinymult.sv`.  For larger designs, pass all the (System)Verilog
source files, followed by the `-o out.cbor` option.

Second, run the `importer` tool, which reads a design from the CBOR file and
produces several outputs, based on a configuration file:

    ./importer examples/tinymult.toml

`examples/tinymult.toml` configures the tool to read a SystemVerilog AST from
`tinymult.cbor` and produce `tinymult/modules/*.dot`, `tinymult/deps.dot`,
`tinymult/tinymult.cfr`, and `tinymult/tinymult.smtlib2` as outputs.  See
`examples/tinymult.toml` for more information on the inputs and outputs of
architecture extraction.
