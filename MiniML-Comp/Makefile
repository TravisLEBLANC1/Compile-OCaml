
.PRECIOUS: minimlc.exe impc.exe # to not destroy the exe

%.exe:
	dune build

% : tests/test_comp/%.ml minimlc.exe impc.exe
	./minimlc.exe tests/test_comp/$@.ml
	./impc.exe tests/test_comp/$@.imp

clean:
	rm -f tests/*.asm tests/*.imp
	dune clean