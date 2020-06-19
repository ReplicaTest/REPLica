build:
	idris2 --build replica.ipkg

clean:
	$(RM) -r build

test:
	build/exec/replica tests/cli/success/001 "./run"
	build/exec/replica tests/cli/failure/001 "./run"
