
all: rules.cmx main.cmx debug.cmx
	ocamlopt -o jerboa 	str.cmxa \
						debug.cmx \
						construction.cmx \
						rules.cmx \
						main.cmx

main.cmx: base.cmi rules.cmx debug.cmx construction.cmx
	ocamlopt -c main.ml

rules.cmx: base.cmi
	ocamlc -c rules.mli
	ocamlopt -c rules.ml

base.cmi:
	ocamlopt -c base.mli

debug.cmx: base.cmi
	ocamlc -c debug.mli
	ocamlopt -c debug.ml

construction.cmx: base.cmi
	ocamlc -c construction.mli
	ocamlopt -c construction.ml

clean:
	rm -rf *.cmi
	rm -rf *.cmx
	rm -rf *.o
	rm -rf jerboa
