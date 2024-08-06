(lang dune 3.0)

(implicit_transitive_deps true)

; Menhir options
(using menhir 2.0)

; Since we want to generate opam files we need to provide informations ;
(generate_opam_files true)

(name mdrp_lib)
(version dev)
(authors "mattiasdrp")
(maintainers "mattiasdrp")
(source (github mattiasdrp/mdrp_lib))
; Not specifying bug_reports since it defaults to the github issue page
(homepage "https://mattiasdrp.github.io/mdrp_lib/")
(documentation "https://mattiasdrp.github.io/mdrp_lib/")

; Packages

(package
 (name mdrp_lib)
 (synopsis "Mdrp Package")
 (description "\
TODO")
 (depends
   ocaml
   dune
  (odoc :with-doc)
 ))

(package
 (name mdrp_cards)
 (synopsis "Mdrp Cards")
 (description "\
TODO")
 (depends
   ocaml
   dune
  (odoc :with-doc)
 ))

(package
 (name mdrp_poker)
 (synopsis "Mdrp Poker")
 (description "\
TODO")
 (depends
   ocaml
   dune
  (odoc :with-doc)
 ))
