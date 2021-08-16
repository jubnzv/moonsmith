(lang dune 2.0)
(using menhir 2.0)

(generate_opam_files true)

(name moonsmith)
(version 0.0.1)
(authors "Georgiy Komarov")
(license LGPL3)
(source (github jubnzv/moonsmith))
(maintainers "Georgiy Komarov <jubnzv@gmail.com>")

(package
  (name moonsmith)
  (synopsis "Random generator of Lua programs."))
