(lang dune 3.4)

(name interval_map)

(generate_opam_files true)

(source
 (github QAhell/interval_map_ocaml))

(authors "Armin Heller")

(maintainers "Armin Heller")

(license "Apache-2.0")

(documentation https://github.com/QAhell/interval_map_ocaml)

(implicit_transitive_deps false)

(package
 (name interval_map)
 (version 0.1)
 (synopsis "An implementation of interval maps")
 (description "An implementation of interval maps")
 (depends ocaml dune)
 (tags (topics interval map)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
