{ zsh, dhall, haskellPackages, buildReplica, replica_dhall }:
let
  inherit (haskellPackages)
    dhall-json;
in
buildReplica.build.overrideAttrs (attrs: {
  buildInputs = [ replica_dhall dhall dhall-json zsh ];
  buildPhase = ''
    cp -r ${replica_dhall}/.cache .cache
    chmod -R u+w .cache
    export XDG_CACHE_HOME=.cache
    make test RUN="-T online"
  '';
});
