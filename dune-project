(lang dune 3.8)

(name Aether)

(generate_opam_files true)

(package
 (name Aether)
 (depends (ocaml(= :5.0.0)) 
          (dune(= :3.8)) 
          (alcotest(= :1.7.0)) 
          (core(= :0.16.0)) 
          (ppx_deriving(= :5.2.1))
          (reason(= :3.8.2))
          (domainlibs(= :0.5.0))
          (stdio(= :0.16.0))
))
