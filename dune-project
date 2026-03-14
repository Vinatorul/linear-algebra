(lang dune 3.0)

(name linear_algebra)

(generate_opam_files true)

(source
 (github vinatorul/linear-algebra))

(authors "Vinatorul")

(maintainers "Vinatorul")

(license MIT)

(package
 (name linear_algebra)
 (synopsis "A library for matrix operations and linear algebra")
 (description "OCaml library providing matrix operations and common linear algebra functions")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (alcotest :with-test)
  (bisect_ppx :with-test)))
