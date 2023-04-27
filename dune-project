(lang dune 3.7)

(name apjo)

(generate_opam_files true)

(source
 (github cvarad/apjo))

(authors "Varad Raut")

(maintainers "Varad Raut")

(license ISC)

(documentation https://url/to/documentation)

(package
 (name apjo)
 (synopsis "Applicative Parser for JSON in OCaml")
 (description
  "A JSON parser written in pure OCaml, without any external dependencies. The parser uses Functional Parsing internally.")
 (depends (ocaml (= 4.12.1)) dune (alcotest :with-test))
 (tags
  (json parser applicative)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
