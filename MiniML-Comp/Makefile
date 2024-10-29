
% : tests/%.ml
	./minimlc.exe tests/$@.ml
	./impc.exe tests/$@.imp

clean:
	rm -f tests/*.asm tests/*.imp
	dune clean