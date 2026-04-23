all: exceptions types parser lexer utils typeops termops main
	ocamlc -g -o top exceptions.cmo types.cmo parser.cmo lexer.cmo utils.cmo typeops.cmo termops.cmo main.cmo
	rm -f lexer.ml parser.mli parser.ml *.cmi *.cmo *~

types: types.mli
	cp types.mli types.ml
	ocamlc -g -c types.mli types.ml
	rm types.ml

exceptions: exceptions.mli
	cp exceptions.mli exceptions.ml
	ocamlc -g -c exceptions.mli exceptions.ml
	rm exceptions.ml

typeops: typeops.ml typeops.mli
	ocamlc -g -c typeops.mli typeops.ml

termops: termops.ml termops.mli
	ocamlc -g -c termops.mli termops.ml

utils: utils.ml utils.mli
	ocamlc -g -c utils.mli utils.ml

parser: parser.mly
	ocamlyacc parser.mly
	ocamlc -g -c parser.mli parser.ml

lexer: lexer.mll
	ocamllex lexer.mll
	ocamlc -g -c lexer.ml

main: main.ml
	ocamlc -g -c main.ml

run: top
	OCAMLRUNPARAM=b ledit ./top

clean:
	rm -f lexer.ml parser.mli parser.ml *.cmi *.cmo *~
