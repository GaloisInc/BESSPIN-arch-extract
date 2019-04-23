# Building

Open a Nix shell with all the dependencies:

    nix-shell --pure nix

Compile the architecture extraction tools:

    make


# `driver` / `besspin-arch-extract`

The `driver` binary corresponds to the `besspin-arch-extract` command in the
BESSPIN tool suite.

Usage:

    ./driver <config.toml> <cmd...>

`<config.toml>` is the path to a configuration file such as the ones in
`examples/`.  `<cmd...>` is a subcommand (documented below) and its arguments.

## Subcommands

For the most complete listing of subcommands, see the `main` function in
`driver.hs`.

### `visualize`

    ./driver <config.toml> visualize

Extracts architectural information from all source files, and renders the
extracted architecture as Graphviz files.  The settings in the `[graphviz]`
config section control the appearance of the output.

### `list-srcs`

    ./driver <config.toml> list-srcs [groups...]

Lists the source files contained in the named source groups.  A source group is
a config section such as `[src.foo]` - in this case, the group name is `foo`.
If no groups are listed, the tool lists files from all groups.

### `list-pp-flags`

    ./driver <config.toml> list-pp-flags [groups...]

Lists Verilog preprocessor flags that appear in an `ifdef` or similar
conditional compilation construct and that are not `define`d within the source
files themselves.  For example:

    `ifdef FOO
    `define BAR
    `endif

    `ifdef BAR
    logic x = `XVAL;
    // Some code ...
    `endif

    `ifdef BAZ
    // Other code ...
    `endif

On this input, the tool will print `FOO` and `BAZ`.  It will not list `XVAL`
because it is not used for conditional compilation.  `BAR` is used for
conditional compilation, but will not be listed because it is (conditionally)
defined in the input itself.  The assumption is that `define`d macros are not
truly parameters of the design, but rather are computed or derived from other
parameters during preprocessing.

### `list-pp-flag-users`

    ./driver <config.toml> list-pp-flag-users [groups... --] [flags...]

Lists the source files from `groups` that use the listed preprocessor flags for
conditional compilation.  If `groups` is omitted, all source groups are
checked.  If `groups` is provided, it must be separated from `flags` by a `--`
argument.

### `bsv-merge-cbor`

    ./driver <config.toml> bsv-merge-cbor <files...>

Reads BSV AST dumps (in CBOR format) from `files`, and combines them into a
single AST dump called `out.cbor`.  The contents of `config.toml` are not
actually used by this command (but it must still be a valid configuration
file).


# `featuresynth/featuresynth.rkt` / `besspin-feature-extract`

The `featuresynth/featuresynth.rkt` script corresponds to the
`besspin-feature-extract` command in the BESSPIN tool suite.

## Subcommands

For the most complete listing of subcommands, see `match` at the bottom of
`featuresynth/featuresynth.rkt`.

### `synthesize`

    racket featuresynth/featuresynth.rkt <config.toml> synthesize

Synthesize a feature model using the `[featuresynth]` settings in
`config.toml`.  See `examples/swerv.toml` for a description of the settings.

The synthesis algorithm tests a variety of configurations to determine which
features of the design are compatible with each other.  It periodically reports
to the console the number of tests run so far, as well as the number of valid
configurations it has discovered (called "positive tests").  The synthesis
process usually requires a broad range of positive tests to complete.  If it
runs for a long time (hundreds of negative tests) without finding a positive
tests, then the synthesis algorithm has probably stalled, and is not likely to
complete without adjustments to the configuration, such as adding a wider
variety of positive tests to `init-tests-file` or lowering the
`boredom-threshold`.  If it finds no positive tests at all, then there is
likely an error with either the `oracle-command` or the `init-tests-file`.

During synthesis, the tool records the outcome of each test into
`./test-log.rktd`.  After synthesis ends or is stopped, this file can be copied
to another location and used as the `resume-tests-file` for a future run.

### `unsat-core`

    racket featuresynth/featuresynth.rkt <config.toml> unsat-core

Compute an unsatisfiable core.  Reads a set of tests from the
`resume-tests-file`, and finds a minimal set of tests and relevant features
that produces an UNSAT result (i.e., synthesis failure).  Based only on the
tests included in the unsat core, the synthesizer can conclude that there is no
feature model (using the `max-groups` and other settings from the config file)
that produces correct results on all the tests.  This output can be useful for
debugging synthesis failures.


# Examples

## LEG

The `examples/leg.toml` config file is used for visualizing the architecture of
the LEG CPU.

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

The `examples/swerv.toml` config file is used for visualizing and synthesizing
feature models for the SWERV CPU.

First, clone the [SWERV repository](https://github.com/westerndigitalcorporation/swerv_eh1/)
into `../swerv_eh1`.

To extract and visualize the architecture of SWERV:

    # Inside nix-shell:
    ./driver examples/swerv.toml visualize

To render the architecture of the `exu_mul_ctl` module:

    dot -Tpdf out/exu_mul_ctl.dot -o out/exu_mul_ctl.pdf

Then open `out/exu_mul_ctl.pdf`.

To extract a feature model for SWERV:

    # Inside nix-shell:
    racket featuresynth/featuresynth.rkt examples/swerv.toml

This will generate a Clafer feature model for SWERV in `swerv.cfr`.  Note this
command may take an hour or more to run.  A pregenerated copy of its output is
available at `examples/swerv-pregen.cfr`.


## Piccolo

The `examples/piccolo.toml` config file is intended for visualizing and
synthesizing feature models for the Piccolo CPU.  However, it currently does
not work very well.  Follow the architecture and feature model extraction
examples from the BESSPIN `tool-suite` repository instead.
