{
  description = "Golden tests for command-line interfaces.";

  inputs.flake-utils.url = github:numtide/flake-utils;
  inputs.idris = {
    type = "github";
    owner = "idris-lang";
    repo = "Idris2";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.flake-utils.follows = "flake-utils";
  };
  inputs.papers = {
    type = "github";
    owner = "idris-lang";
    repo = "Idris2";
    dir = "libs/papers";
    flake = false;
  };
  outputs = { self, nixpkgs, idris, papers, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      npkgs = import nixpkgs { inherit system; };
      inherit (npkgs) dhall;
      inherit (npkgs.haskellPackages) dhall-json;
      idrisPkgs = idris.packages.${system};
      buildIdris = idris.buildIdris.${system};
      papersPkg = buildIdris {
        projectName = "papers";
        src = papers;
        idrisLibraries = [];
        preBuild = "cd libs/papers";
      };
      my-papers = papersPkg.installLibrary;
      pkgs = buildIdris {
        projectName = "replica";
        src = ./.;
        idrisLibraries = [ my-papers ];
        preBuild = ''
          make
        '';
      };
    in rec {
      packages = papersPkg // pkgs // idrisPkgs;
      defaultPackage = pkgs.build;
      devShell = npkgs.mkShell {
        packages = [ idrisPkgs.idris2 npkgs.rlwrap dhall dhall-json ];
        inputsFrom = [ papers ];

        shellHook = ''
          # awful hack to get the papers package in nix develop
          pushd ${papers}/libs/papers
          idris2 --install papers.ipkg --build-dir /tmp
          popd
          alias idris2="rlwrap -s 1000 idris2 --no-banner"
        '';
      };
    }
  );
}
