(lang dune 3.3)

(name camley)

(generate_opam_files true)

(source
 (github fvdveen/camley))

(authors "Friso van der Veen")

(maintainers "Friso van der Veen")

(license MIT)

(package
 (name camley)
 (synopsis "A monkey implementation in ocaml")
 (description "A monkey implementation in ocaml")
 (depends ocaml dune)
 (tags
  (compiler)))

(using menhir 2.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
