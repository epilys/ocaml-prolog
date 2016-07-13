all:
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core -pkg str main.native
