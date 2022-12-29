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
      inherit (npkgs) dhall;
      inherit (npkgs.haskellPackages) dhall-json;
      inherit (npkgs) zsh;
      idrisPkgs = idris.packages.${system};
      buildIdris = idris.buildIdris.${system};
      papersPkg = buildIdris {
        projectName = "papers";
        src = papers;
        idrisLibraries = [];
        preBuild = "cd libs/papers";
      };
      my-papers = papersPkg.installLibrary;
      replica_ = buildIdris {
        projectName = "replica";
        src = ./.;
        idrisLibraries = [ my-papers ];
      };
      replicaPatched = replica_.build.overrideAttrs (attrs: {
        patchPhase = ''
          sed "s/\`git describe --tags\`/unknown-${self.shortRev or "dirty"}/" -i Makefile
        '';
      });
      replica = replicaPatched.overrideAttrs (attrs: {
        buildPhase = ''
          make
        '';
      });
      replicaTest = replicaPatched.overrideAttrs (attrs: {
        buildInputs = [ dhall dhall-json zsh ];
        buildPhase = ''
          export REPLICA_DHALL="$PWD/submodules/replica-dhall/package.dhall"
          export DHALL_PRELUDE="$PWD/submodules/dhall-lang/Prelude/package.dhall"
          XDG_CACHE_HOME=`mktemp -d` make test
        '';
      });
      dockerImage = npkgs.dockerTools.buildImage {
         name = "replica";
         config = {
            Cmd = [ "${replica}/bin/replica" ];
         };
         tag = "latest";
      };
    in rec {

      packages = {
        default = replica;
        docker = dockerImage;
      };

      checks.tests = replicaTest;

      devShells.default = npkgs.mkShell {
        packages = [ idrisPkgs.idris2 npkgs.rlwrap dhall dhall-json ];
        inputsFrom = [ papers ];

        shellHook = ''
          # awful hack to get the papers package in nix develop
          pushd ${papers}/libs/papers
          idris2 --install papers.ipkg --build-dir `mktemp -d` > /dev/null
          popd
          alias idris2="rlwrap -s 1000 idris2 --no-banner"
        '';
      };
    }
  );
}
