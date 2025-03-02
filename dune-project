(lang dune 3.16)
(using menhir 2.0)

(name mimosa)

(generate_opam_files true)

(source (github nikolaushuber/mimosa))
(authors "Nikolaus Huber")
(maintainers "Nikolaus Huber")

(license MIT)

(documentation nikolaushuber.github.io/mimosa/)

(package
 (name mimosa)
 (synopsis "The Mimosa tool")
 (description "Compiler and simulator for the Mimosa language")
 (depends
  ocaml
  dune
  menhir
  fmt
  pp_loc
  tsort
  sexplib
  ppxlib
  cmdliner
  minttea
  leaves
  spices))
