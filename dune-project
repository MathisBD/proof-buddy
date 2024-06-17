(lang dune 3.13)

(using menhir 2.0)

(name proof-buddy)

; The .opam file is regenerated each time you call dune build.
; To manually regenerate the .opam file, run :
; $ dune build proof-buddy.opam

(generate_opam_files true)

(package
 (name proof-buddy)
 (maintainers "Mathis Bouverot-Dupuis (mathis.bouverot@ens.psl.eu)")
 (authors "Mathis Bouverot-Dupuis")
 (homepage "https://github.com/MathisBD/proof-buddy")
 (bug_reports "https://github.com/MathisBD/proof-buddy/issues")
 (synopsis "A toy proof assistant")
 (depends
  (batteries
   (>= 3.8.0))
  (ppx_deriving
   (>= 5.2.1))))
