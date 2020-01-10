
all: rules.cmx main.cmx debug.cmx 
	ocamlopt -o jerboa 	str.cmxa \
						debug.cmx \
						util.cmx \
						construction.cmx \
						rules.cmx \
						lexer.cmx \
						parser.cmx \
						main.cmx

main.cmx: base.cmi rules.cmx debug.cmx construction.cmx lexer.cmx parser.cmx 
	ocamlopt -c main.ml

rules.cmx: base.cmi
	ocamlc -c rules.mli
	ocamlopt -c rules.ml

base.cmi:
	ocamlopt -c base.mli

debug.cmx: base.cmi
	ocamlc -c debug.mli
	ocamlopt -c debug.ml

construction.cmx: base.cmi util.cmx
	ocamlc -c construction.mli
	ocamlopt -c construction.ml

lexer.cmx: base.cmi construction.cmx
	ocamlc -c lexer.mli
	ocamlopt -c lexer.ml

parser.cmx: base.cmi util.cmx construction.cmx
	ocamlc -c parser.mli
	ocamlopt -c parser.ml

util.cmx: 
	ocamlc -c util.mli
	ocamlopt -c util.ml

clean:
	rm -rf *.cmi
	rm -rf *.cmx
	rm -rf *.cmo
	rm -rf *.o
	rm -rf jerboa
