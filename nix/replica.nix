{ buildReplica, version }:
buildReplica.build.overrideAttrs (attrs: {
  pname = "replica";
  version = version;
  buildPhase = ''
    make
  '';
})
