(lang dune 3.7)

(name interpreter)

(generate_opam_files true)

(package
 (name interpreter)
 (depends ocaml dune batteries alcotest core ppx_deriving))
