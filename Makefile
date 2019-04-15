
simchk: simchk.ml vector.ml string_distance.ml
	ocamlc -i -c vector.ml > vector.mli
	ocamlc -c vector.mli
	ocamlc -i -c string_distance.ml > string_distance.mli
	ocamlc -c string_distance.mli
	ocamlc -i -c str.cma simchk.ml > simchk.mli
	ocamlc -c simchk.mli
	ocamlopt -o simchk str.cmxa vector.ml string_distance.ml simchk.ml

clean:
	rm -f *.mli *.cmi *.cmo *.cmx *.o simchk
