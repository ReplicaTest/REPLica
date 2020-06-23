build:
	idris2 --build replica.ipkg

install:
	idris2 --install replica.ipkg

clean:
	$(RM) -r build

test:
	build/exec/replica tests
