{
  description = "Haskell development environment";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    {
      self,
      flake-utils,
      nixpkgs,
    }:
    let
      theseHpkgNames = [
        "magix"
      ];
      hOverlay = selfn: supern: {
        haskell = supern.haskell // {
          packageOverrides =
            selfh: superh:
            supern.haskell.packageOverrides selfh superh
            // {
              # The sandbox has no nix binary for test-magix-nix.
              magix = supern.haskell.lib.overrideCabal (selfh.callCabal2nix "magix" ./. { }) (_: {
                testTargets = [ "test-magix" ];
              });
            };
        };
      };
      overlays = [ hOverlay ];
      perSystem =
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            inherit overlays;
          };
          hpkgs = pkgs.haskellPackages;
          hlib = pkgs.haskell.lib;
          theseHpkgs = nixpkgs.lib.genAttrs theseHpkgNames (n: hpkgs.${n});
          theseHpkgsDev = builtins.mapAttrs (_: x: hlib.doBenchmark x) theseHpkgs;
        in
        {
          packages = rec {
            default = magix;
            magix = hlib.justStaticExecutables theseHpkgs.magix;
          };

          devShells.default = hpkgs.shellFor {
            packages = _: (builtins.attrValues theseHpkgsDev);
            nativeBuildInputs = [
              # Haskell toolchain.
              hpkgs.cabal-fmt
              hpkgs.cabal-install
              hpkgs.haskell-language-server
            ];
            buildInputs = [ ];
            doBenchmark = true;
            withHoogle = true;

            NIX_PATH = "nixpkgs=${nixpkgs}";
            shellHook = ''
              export PATH="$PWD/scripts:$PATH"
            '';
          };

          # Build every test script as a consumer's check would.
          checks =
            let
              inherit (pkgs) lib;
              buildScripts =
                language:
                let
                  dir = ./test-scripts + "/${language}";
                in
                lib.mapAttrs' (n: _: {
                  name = "${language}-${lib.replaceStrings [ " " ] [ "-" ] n}";
                  value = self.lib.build {
                    inherit pkgs;
                    script = dir + "/${n}";
                  };
                }) (builtins.readDir dir);
              empty = ./test-scripts/bash/empty;
            in
            lib.concatMapAttrs (n: _: buildScripts n) (builtins.readDir ./test-scripts)
            // {
              # The executable is named after a path, a store path, or 'name'.
              lib-build-names = pkgs.runCommand "lib-build-names" { } ''
                test -x ${
                  self.lib.build {
                    inherit pkgs;
                    script = empty;
                  }
                }/bin/empty
                test -x ${
                  self.lib.build {
                    inherit pkgs;
                    script = "${empty}";
                  }
                }/bin/empty
                test -x ${
                  self.lib.build {
                    inherit pkgs;
                    script = empty;
                    name = "other.sh";
                  }
                }/bin/other
                touch "$out"
              '';
            };

          devShells.check = hpkgs.shellFor {
            packages = _: [ theseHpkgs.magix ];
            buildInputs = [ hpkgs.cabal-install ];
            NIX_PATH = "nixpkgs=${nixpkgs}";
            shellHook = ''
              export PATH="$PWD/scripts:$PATH"
            '';
          };
        };
    in
    {
      overlays.default = nixpkgs.lib.composeManyExtensions overlays;

      # See 'Check scripts in a flake' in Readme.md.
      lib.build =
        {
          pkgs,
          script,
          name ?
            let
              base = baseNameOf (toString script);
              m = builtins.match "[0-9a-z]{32}-(.*)" base;
            in
            if m == null then base else builtins.head m,
        }:
        let
          # A store path cannot carry the name of every script.
          src =
            if builtins.isPath script then
              builtins.path {
                path = script;
                name = "script";
              }
            else
              script;
          expression =
            pkgs.runCommand "${pkgs.lib.strings.sanitizeDerivationName name}-magix-expression"
              {
                inherit src;
                scriptName = name;
                nativeBuildInputs = [ self.packages.${pkgs.stdenv.buildPlatform.system}.magix ];
              }
              ''
                # The layout of a build directory of magix.
                mkdir "$out"
                cp "$src" "$out/script"
                cp "$src" "./$scriptName"
                magix print-magix-expression "./$scriptName" >"$out/default.nix"
              '';
        in
        import expression { inherit pkgs; };
    }
    // flake-utils.lib.eachDefaultSystem perSystem;

  nixConfig = {
    extra-substituters = [
      "https://dschrempf-magix.cachix.org"
    ];
    extra-trusted-public-keys = [
      "dschrempf-magix.cachix.org-1:cScG6NjZBiQvY7KjSPpQdUa9UXZVLz9rvUZHtuwdYwc="
    ];
  };
}
