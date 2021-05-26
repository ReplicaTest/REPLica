# with this rule if x.dhall exists but not x.json, and x.json is a target
.dhall.json:
	dhall-to-json --file $? --output $@

## Example usage
test: mytests.json
	replica run mytests.json

# Do not forget to clean the json file on clean
clean:
	${RM} mytests.json
