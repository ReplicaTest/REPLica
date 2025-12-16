# Error codes

A list of `replica` error codes:

| Error Code | Meaning |
|:----------:|:--------|
| 0 | Success |
| 1 to 127 | Number of failing tests (wrong expectation or test error) |
| 128 | More than 127 failing tests |
| 252 | Incompatible options (eg. including and excluding the same tag) |
| 253 | Invalid option |
| 254 | Invalid JSON test file |
| 255 | Cant read/write a file |
