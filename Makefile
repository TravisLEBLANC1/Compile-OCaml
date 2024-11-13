
.PRECIOUS: bin/minimlc.exe bin/impc.exe # to not destroy the exe
MARS=java -jar ../../Mars/Mars4_5.jar

%.exe:
	@dune build

% : tests/test_comp/%.ml minimlc.exe
	@./bin/minimlc.exe tests/test_comp/$@.ml
	@./bin/impc.exe tests/test_comp/$@.imp
	@$(MARS) tests/test_comp/$@.asm

clean:
	rm -f tests/*/*.asm tests/test_comp/*.imp tests/test_typecheck/*.imp
	dune clean