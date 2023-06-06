(lang dune 3.7)

(name aether)

(generate_opam_files true)

(package
 (name aether)
 (depends ocaml dune batteries alcotest core ppx_deriving))
