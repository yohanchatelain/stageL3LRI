CMA=str.cma
CMO=lexer.cmo parser.cmo static_analyze.cmo sct.cmo main.cmo
GENERATED = lexer.ml parser.ml parser.mli
BIN=sct
FLAGS=-annot
all: $(BIN)
	./$(BIN) test.sct

$(BIN):$(CMO)	
	ocamlc $(FLAGS) -o $(BIN) $(CMA) $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .cma .mll .mly

.mli.cmi:
	ocamlc $(FLAGS) -c  $<

.ml.cmo:
	ocamlc $(FLAGS) -c  $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir -v $<

clean:
	rm -f *.cm[io] *.o *~ $(BIN) $(GENERATED) parser.output

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend



