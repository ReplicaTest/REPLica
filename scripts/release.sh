sed "s/version = .*/version = $1/" -i replica.ipkg
echo "\"$1\"" > ./version.nix
git commit replica.ipkg ./version.nix -m "Set up release v$1"
