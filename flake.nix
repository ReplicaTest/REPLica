{
  description = "Golden tests for command-line interfaces.";

  inputs.flake-utils.url = github:numtide/flake-utils;
  inputs.idris = {
    type = "github";
    owner = "berewt";
    repo = "Idris2";
    ref = "flake-update";
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
      inherit (npkgs)
        dhall
        zsh;
      inherit (npkgs.haskellPackages)
        dhall-json;
      idrisPkgs = idris.packages.${system};
      buildIdris = idris.buildIdris.${system};

      version = import ./version.nix;

      papersPkg = buildIdris {
        projectName = "papers";
        src = papers;
        idrisLibraries = [];
        preBuild = "cd libs/papers";
      };
      papersLib = papersPkg.installLibrary;

      replica_ = buildIdris {
        projectName = "replica";
        src = ./.;
        idrisLibraries = [ papersLib ];
      };
      replica = replica_.build.overrideAttrs (attrs: {
        pname = "replica";
        version = version;
        buildPhase = ''
          make
        '';
      });

      replicaTest = replica_.build.overrideAttrs (attrs: {
        buildInputs = [ dhall dhall-json zsh ];
        buildPhase = ''
          REPLICA_DHALL="$PWD/submodules/replica-dhall/package.dhall" \
          DHALL_PRELUDE="$PWD/submodules/dhall-lang/Prelude/package.dhall" \
          XDG_CACHE_HOME=`mktemp -d` make test
        '';
      });

      dockerImage = npkgs.dockerTools.buildImage {
         name = "replica";
         config = {
            Cmd = [ "${replica}/bin/replica" ];
         };
         tag = "v${version}";
      };

    in rec {

      packages = {
        default = replica;
        docker = dockerImage;
      };

      checks.tests = replicaTest;

      devShells.default = npkgs.mkShell {
        packages = [ idrisPkgs.idris2 papersLib npkgs.rlwrap dhall dhall-json ];
        shellHook = ''
          alias idris2="rlwrap -s 1000 idris2 --no-banner"
        '';
      };

    }
  );
}
