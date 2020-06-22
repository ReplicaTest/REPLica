build:
	idris2 --build replica.ipkg

clean:
	$(RM) -r build

test:
	build/exec/replica tests/cli/success/001 tests/cli/failure/001
