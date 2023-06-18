(lang dune 3.7)

(name Aether)

(generate_opam_files true)

(package
 (name Aether)
 (depends (ocaml(= :5.0.0)) 
          (dune(= :3.7)) 
          (alcotest(= :1.7.0)) 
          (core(= :0.15.1)) 
          (ppx_deriving(= :5.2.1))
          (reason(= :3.8.2))
          (domainlibs(= :0.5.0))
))
