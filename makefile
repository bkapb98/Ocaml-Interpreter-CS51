all:
	ocamlbuild -use-ocamlfind evaluation.byte
	ocamlbuild -use-ocamlfind expr.byte
	ocamlbuild -use-ocamlfind tests.byte
	ocamlbuild -use-ocamlfind miniml.byte
