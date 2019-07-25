set -e # stop on first error
qtest -o CalcSolverTests.ml extract CalcSolver.ml
ocamlbuild -cflags -warn-error,+26 -use-ocamlfind -package oUnit -package qcheck CalcSolverTests.native
./CalcSolverTests.native
