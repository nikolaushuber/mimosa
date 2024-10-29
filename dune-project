(lang dune 3.16)
(using menhir 2.0)

(name mimosa)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Nikolaus Huber")

(maintainers "Nikolaus Huber")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name mimosa)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  menhir
  cmdliner
  fmt
  pp_loc
  tsort)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
