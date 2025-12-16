{ buildIdris, papersLib }:
buildIdris {
  ipkgName = "replica";
  src = ../.;
  idrisLibraries = [ papersLib ];
}
