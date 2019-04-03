# Building

Open a Nix shell with all the dependencies:

    nix-shell --pure nix

Compile the architecture extraction tools:

    make


# Running examples

## LEG

First, clone the [LEG repository](https://github.com/MWaug/LEG) into `../LEG`.

To extract and visualize the architecture of LEG:

    # Inside nix-shell:
    ./driver examples/leg.toml visualize

This will generate Graphviz files `out/*.dot`, one for each module of the LEG
design.

To render the architecture of the `ahb_lite` module:

    dot -Tpdf out/ahb_lite.dot -o out/ahb_lite.pdf

Then open `out/ahb_lite.pdf`.

To see other views of the architecture, modify the `[graphviz]` section of
`examples/leg.toml` and re-run the commands above.

The LEG CPU has no configurable features, so there is no demo of feature model
extraction for it.


## SWERV

First, clone the [SWERV repository](https://github.com/westerndigitalcorporation/swerv_eh1/)
into `../swerv_eh1`.

To extract and visualize the architecture of SWERV: **TODO: test this**

    # Inside nix-shell:
    ./driver examples/swerv.toml visualize

To render the architecture of the `exu_mul_ctl` module:

    dot -Tpdf out/exu_mul_ctl.dot -o out/exu_mul_ctl.pdf

Then open `out/exu_mul_ctl.pdf`.

To extract a feature model for SWERV: **TODO: test this**

    # Inside nix-shell:
    racket featuresynth/featuresynth.rkt examples/swerv.toml

This will generate a Clafer feature model for SWERV in `swerv.cfr`.  Note this
command may take an hour or more to run.  A pregenerated copy of its output is
available at `examples/swerv-pregen.cfr`.


## Piccolo

First, clone the [GFE repository](https://gitlab-ext.galois.com/ssith/gfe) into
`../gfe`.

Extracting an architecture from BSV code currently requires manually building
the project using [a modified version of `bsc`](
https://gitlab-ext.galois.com/bsc_src_access/bsc_src/commits/ast-export).  A
compiled copy of the modified `bsc` is available on the BESSPIN VM at **TODO**.

To prepare the AST files for architecture extraction: **TODO: test this**

    # In the Piccolo directory
    cd builds/RV32ACIMU_Piccolo_verilator
    mkdir -p out
    rm -f build_dir/*
    PATH=... make compile   # TODO: path to modified `bsc`

    # In the arch-extract directory, inside nix-shell
    ./driver examples/piccolo.toml bsv-merge-cbor \
        ../gfe/bluespec-processors/P1/Piccolo/builds/RV32ACIMU_Piccolo_verilator/out/tc.*.bsv.cbor
    mv out.cbor piccolo.cbor

This will produce a file `piccolo.cbor` containing a representation of the
Piccolo source code.

To visualize the Piccolo architecture: **TODO: test this**

    # Inside nix-shell
    ./driver examples/piccolo-arch.toml visualize

To render the architecture of the `Shifter_Box.mkShifter_Box` module:

    dot -Tpdf out/Shifter_Box.mkShifter_Box.dot -o out/Shifter_Box.mkShifter_Box.pdf

Then open `out/Shifter_Box.mkShifter_Box.pdf`.

BSV architecture extraction is currently incomplete, and may fail to detect
instantiations of some modules, particularly modules from the BSV standard
library.

To extract a feature model for Piccolo: **TODO: doesn't work**

    # Inside nix-shell:
    racket featuresynth/featuresynth.rkt examples/piccolo.toml

However, feature model synthesis for Piccolo currently fails.  After running
for several hours, it will eventually print the lines:

    VOTE-QUIT: strategy 1 ((distinguish ...))
    VOTE-QUIT: strategy 2 ((disprove ...))

Afterward, it will continue running, but will no longer make any progress.
