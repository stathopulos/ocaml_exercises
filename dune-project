(lang dune 3.16)

(name ocaml_exercises)

(generate_opam_files true)

(source
 (github stathopulos/ocaml_exercises))

(authors "Konstantinos Stathopulos")

(package
 (name ocaml_exercises)
 (description "Solutions for the exercises from the OCaml website")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
