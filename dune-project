(lang dune 3.8)

(name aether)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name aether)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends (ocaml(= :5.0.0)) 
          (dune(= :3.8)) 
          (alcotest(= :1.7.0)) 
          (core(= :0.16.0)) 
          (ppx_deriving(= :5.2.1))
          (reason(= :3.9.0))
          (domainlibs(= :0.5.0))
          (stdio(= :0.16.0))
 )
 (tags
  (topics "to describe" your project))
)
