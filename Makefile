build:
	idris2 --build replica.ipkg:w

clean:
	$(RM) -r build

test:
	build/exec/replica tests/cli/success/001 "./run"
