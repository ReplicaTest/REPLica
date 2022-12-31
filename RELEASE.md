To set up an new release, you can use
[`scripts/release.sh`](./scripts/release.sh).
The script:
1. set up the version field of `replica.ipkg` to the given version number
2. set the version in `version.nix` to the given version number
3. commit these two files with a "Set up release message"
