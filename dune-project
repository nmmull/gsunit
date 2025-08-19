(lang dune 3.12)

(name gsunit)

(generate_opam_files true)

(source (github nmmull/gsunit))

(authors "Nathan Mull")

(maintainers "Nathan Mull")

(package
 (name gsunit)
 (synopsis "Ocaml autograders")
 (description "A library built on top of OUnit2 for writing Gradescope autograders to test OCaml code")
 (depends ocaml dune ounit2 yojson fmt)
 (tags (topics gradescope autograders "unit tests")))
