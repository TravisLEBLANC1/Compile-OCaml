
.PRECIOUS: bin/minimlc.exe bin/impc.exe # to not destroy the exe
MARS=java -jar ../Mars/Mars4_5.jar

%.exe:
	@dune build

%.ml : tests/test_comp/%.ml minimlc.exe
	@./bin/minimlc.exe tests/test_comp/$@
	@./bin/impc.exe tests/test_comp/$*.imp
	@$(MARS) tests/test_comp/$*.asm

%.imp : tests/test_imp/%.imp minimlc.exe
	@./bin/impc.exe tests/test_imp/$@
	@$(MARS) tests/test_imp/$*.asm

clean:
	rm -f tests/*/*.asm tests/test_comp/*.imp tests/test_typecheck/*.imp
	dune clean