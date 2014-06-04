CMA=str.cma
CMO=lexer.cmo parser.cmo infinity.cmo static_analyze.cmo  \
    term.cmo substitution.cmo pretty.cmo sct.cmo main.cmo
GENERATED = lexer.ml parser.ml parser.mli 
BIN=sct
FLAGS=-annot 
all: $(BIN)
	./$(BIN) ../example_sct/comb_size.sct
	./$(BIN) ../example_sct/ack.sct
	./$(BIN) ../example_sct/mutuel_rec1.sct
	./$(BIN) ../example_sct/indirect_rec.sct
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



