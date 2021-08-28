# moonsmith

moonsmith is a generator of random Lua 5.3 programs. It could be used to evaluate tooling that works with Lua.

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

See output of the `--help` option to get the complete set of command line options.

To perform evaluation of your tool, that works with Lua, it may be convenient to write a script that generates a random program and runs your tool over it to check output or return code. You can check example of such script in the test suite in *test* directory.

If you also need some complicated configuration, (for example, you want to disable some Lua constructions which your tooling doesn't support yet), please check *config.ml* file in the source tree. You can disable some options there and recompile the project to get results. These values intentionally weren't moved to the configuration file or CLI arguments, becuase not many users may want such subtle configuration.
