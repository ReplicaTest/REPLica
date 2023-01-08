{
  description = "Golden tests for command-line interfaces.";

  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    idris = {
      url = "github:idris-lang/Idris2";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    papers = {
      url = "github:idris-lang/Idris2?dir=libs/papers";
      flake = false;
    };
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    replicadhall.url = "github:ReplicaTest/replica-dhall";
  };
  outputs = { self, nixpkgs, idris, papers, flake-utils, pre-commit-hooks, replicadhall }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        inherit (pkgs)
          dhall
          zsh;
        inherit (pkgs.haskellPackages)
          dhall-json;
        idrisPkgs = idris.packages.${system};
        replica_dhall = replicadhall.packages.${system}.default;

        buildIdris = idris.buildIdris.${system};

        version = import ./version.nix;

        papersPkg = buildIdris {
          projectName = "papers";
          src = papers;
          idrisLibraries = [ ];
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
          buildInputs = [ replica_dhall dhall dhall-json zsh ];
          buildPhase = ''
            cp -r ${replica_dhall}/.cache .cache
            chmod -R u+w .cache
            export XDG_CACHE_HOME=.cache
            make test RUN="-T online"
          '';
        });

        dockerImage = pkgs.dockerTools.buildImage {
          name = "replica";
          config = {
            Cmd = [ "${replica}/bin/replica" ];
          };
          tag = "v${version}";
        };

      in
      rec {
        packages = {
          default = replica;
          docker = dockerImage;
        };

        checks = {
          tests = replicaTest;
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              nixpkgs-fmt.enable = true;
              dhall-format.enable = true;
              markdownlint.enable = true;
              online-tests = {
                name = "REPLica online tests";
                description = ''
                  We don't run online tests in the CI, so we check
                  it in pre-commit
                '';
                enable = true;
                entry = ''
                  make test RUN="-t online"
                '';
                pass_filenames = false;
                stages = [ "push" ];
              };
            };
          };
        };

        devShells.default = pkgs.mkShell {
          packages = [ idrisPkgs.idris2 papersLib pkgs.rlwrap dhall dhall-json ];
          shellHook = ''
            alias idris2="rlwrap -s 1000 idris2 --no-banner"
            ${self.checks.${system}.pre-commit-check.shellHook}
          '';
        };

      }
    );
}
