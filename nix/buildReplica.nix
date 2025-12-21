{ buildIdris }:
buildIdris {
  ipkgName = "replica";
  src = ../.;
  idrisLibraries = [ ];
}
