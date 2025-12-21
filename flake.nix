{
  description = "Golden tests for command-line interfaces.";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    replicadhall.url = "github:ReplicaTest/replica-dhall";
  };
  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks, replicadhall }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        npkgs = import nixpkgs { inherit system; };
        inherit (npkgs)
          dhall
          idris2
          lib;
        inherit (npkgs.haskellPackages)
          dhall-json;

        version = import ./version.nix;

        callPackage = lib.callPackageWith (npkgs // packages);

        packages = {
          inherit version;
          replica_dhall = replicadhall.packages.${system}.default;
          buildReplica = callPackage ./nix/buildReplica.nix { };
          replica = callPackage ./nix/replica.nix { };
          replicaTest = callPackage ./nix/replica.nix { };
        };

        inherit (packages)
          replica
          replicaTest;

        dockerImage = npkgs.dockerTools.buildImage {
          name = "replica";
          config = {
            Cmd = [ "${replica}/bin/replica" ];
          };
          tag = "v${version}";
        };

      in
      {
        packages = {
          default = replica;
          replica = replica;
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
              online-tests = import ./nix/online-tests.nix;
            };
          };
        };

        devShells.default = npkgs.mkShell {
          packages = [ idris2 npkgs.rlwrap dhall dhall-json ];
          shellHook = ''
            alias idris2="rlwrap -s 1000 idris2 --no-banner"
            ${self.checks.${system}.pre-commit-check.shellHook}
          '';
        };

      }
    );
}
