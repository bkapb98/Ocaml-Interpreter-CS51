all:
	ocamlbuild -use-ocamlfind evaluation.byte
	ocamlbuild -use-ocamlfind expr.byte
