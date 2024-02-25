(lang dune 3.8)

(using melange 0.1)

(name melange-tea)

(license "MIT")

(maintainers "Eyal Chojnowski")

(authors "Eyal Chojnowski")

(homepage https://github.com/noomly/melange-tea)

(bug_reports https://github.com/noomly/melange-tea/issues)

(generate_opam_files)

(package
 (name melange-tea)
 (synopsis "The Elm Architecture for Melange")
 (description "Port of Bucklescript-tea for Melange")
 (depends
  (ocaml (>= "5.1"))
  (melange (>= "3.0.0"))
  melange-webapi
  (reason (>= "3.10"))
  ocamlformat
  (ocaml-lsp-server :with-test)))
