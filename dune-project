(lang dune 3.0)
(using menhir 2.1)

(name BiOCamLib)

(generate_opam_files true)

(source
 (github PaoloRibeca/BiOCamLib))

(authors "Paolo Ribeca")

(maintainers "Paolo Ribeca")

(license LICENSE)

(documentation https://github.com/PaoloRibeca/BiOCamLib#readme)

(package
 (name BiOCamLib)
 (synopsis "A foundational OCaml library for bioinformatics")
 (description "The OCaml foundations upon which a number of the bioinformatics tools I developed are built")
 (depends ocaml dune)
 (tags
  (topics bioinformatics OCaml)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
