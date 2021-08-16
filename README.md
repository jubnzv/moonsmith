# Moonsmith

Moonsmith is a generator of random Lua programs. It could be used to evaluate tooling that works with Lua.

This project is not ready yet and is still in development.

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
