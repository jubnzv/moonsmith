(lang dune 2.0)
(using menhir 2.0)

(generate_opam_files true)

(name moonsmith)
(version 0.1.0)
(authors "Georgiy Komarov")
(license LGPL-3.0-or-later)
(source (github jubnzv/moonsmith))
(maintainers "Georgiy Komarov <jubnzv@gmail.com>")

(package
  (name moonsmith)
  (synopsis "Random generator of Lua programs")
  (description "This tool can be used to evaluate tooling that works with Lua: parsers, transpilers, code analyzers, etc.")
  (depends
      (ocaml (>= 4.08))
      (fileutils :build)
      (core_kernel :build)
      (clap :build)
      (ppxlib :build)
      (ppx_deriving :build)
      (ppx_deriving_yojson :build)
      (yojson :build)))
