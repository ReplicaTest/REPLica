{ buildIdris, papersLib }:
buildIdris {
  projectName = "replica";
  src = ../.;
  idrisLibraries = [ papersLib ];
}
