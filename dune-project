(lang dune 3.8)

(name briar)

(generate_opam_files true)

(source
 (github dwclake/briar))

(authors "Devon Webb")

(maintainers "Devon Webb")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name briar)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends (ocaml(= :5.0.0)) 
          (dune(= :3.8)) 
          (alcotest(= :1.7.0)) 
          (core(= :0.16.0)) 
          (ppx_deriving(= :5.2.1))
          (domainslib(= :0.5.0))
          (stdio(= :.0.16.0))
          (clap(= :0.3.0))
 )
 (tags
  (topics "to describe" your project))
)
