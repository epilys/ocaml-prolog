all:
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core main.native
