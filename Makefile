all : projet jeu

jeu : projet_comparaison.o projet_proba.o jeu.o
	ocamlc -o jeu projet_comparaison.cmo projet_proba.cmo jeu.cmo

projet :  projet_comparaison.o projet_proba.o compute.o
	ocamlc -o compute str.cma projet_comparaison.cmo projet_proba.cmo compute.cmo

projet_comparaison.o : projet_comparaison.ml
	ocamlc -c projet_comparaison.ml

projet_proba.o : projet_proba.ml
	ocamlc -c projet_proba.ml

compute.o : compute.ml
	ocamlc -c compute.ml

jeu.o : jeu.ml
	ocamlc -c jeu.ml


clean :
	rm $(all) *~ *.cmi *.cmo jeu compute
