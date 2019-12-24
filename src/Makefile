
all: rules.cmx main.cmx
	ocamlopt -o jerboa str.cmxa rules.cmx main.cmx

main.cmx: base.cmi rules.cmx
	ocamlopt -c main.ml

rules.cmx: base.cmi
	ocamlc -c rules.mli
	ocamlopt -c rules.ml

base.cmi:
	ocamlopt -c base.mli

clean:
	rm -rf *.cmi
	rm -rf *.cmx
	rm -rf *.o
	rm -rf jerboa
