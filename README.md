# Moonsmith

*This project is not ready yet and is still in active development.*

Moonsmith is a generator of random Lua programs.

It could be used to evaluate tooling that works with Lua.

Currently it supports Lua 5.1.

## Installation

Install the latest OCaml compiler and opam. Consider installation instructions at [ocaml.org](https://ocaml.org/docs/install.html) and [opam.ocaml.org](https://opam.ocaml.org/doc/Install.html).

Then install required dependencies:

```bash
opam install --deps-only .    # first time only
```

Build and install `moonsmith` binary to the `output` directory:

```bash
dune build @install
mkdir output
dune install --prefix ./output
```

You'll get compiled binary at `output/bin/moonsmith`.

## Usage

You can simply call binary to get randomly-generated Lua program at `out.lua`:

```bash
output/bin/moonsmith
```

See `--help` option to get the complete set of command line options.
