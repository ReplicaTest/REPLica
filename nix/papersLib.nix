{ system, buildIdris, papers }:
let
  version = import ../version.nix;
  papersPkg = buildIdris {
    ipkgName = "papers";
    src = papers;
    idrisLibraries = [ ];
  };
in
papersPkg.library { }
