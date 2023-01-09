{ system, buildIdris, papers }:
let
  version = import ../version.nix;
  papersPkg = buildIdris {
    projectName = "papers";
    src = papers;
    idrisLibraries = [ ];
    preBuild = "cd libs/papers";
  };
in
papersPkg.installLibrary
