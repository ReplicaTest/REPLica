{ buildReplica, version }:
buildReplica.build.overrideAttrs (attrs: {
  pname = "replica";
  version = version;
  __intentionallyOverridingVersion = true;
  buildPhase = ''
    make
  '';
})
