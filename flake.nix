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
      # NOTE: When updating GHC, do not forget to also update the CICD script.
      thisGhcVersion = "ghc910";
      hOverlay = selfn: supern: {
        haskell = supern.haskell // {
          packageOverrides =
            selfh: superh:
            supern.haskell.packageOverrides selfh superh
            // {
              magix = selfh.callCabal2nix "magix" ./. { };
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
          hpkgs = pkgs.haskell.packages.${thisGhcVersion};
          hlib = pkgs.haskell.lib;
          theseHpkgs = nixpkgs.lib.genAttrs theseHpkgNames (n: hpkgs.${n});
          theseHpkgsDev = builtins.mapAttrs (_: x: hlib.doBenchmark x) theseHpkgs;
        in
        {
          packages = theseHpkgs // {
            default = theseHpkgs.magix;
            magix = theseHpkgs.magix;
          };

          devShells.default = hpkgs.shellFor {
            packages = _: (builtins.attrValues theseHpkgsDev);
            nativeBuildInputs = [
              # Haskell toolchain.
              hpkgs.cabal-fmt
              hpkgs.cabal-install
              # HACK: Use custom-built Haskell Language Server (see below).
              hpkgs.ormolu
              # hpkgs.haskell-language-server
            ];
            buildInputs = [ ];
            doBenchmark = true;
            withHoogle = true;

            NIX_PATH = "nixpkgs=${nixpkgs}";
            shellHook = ''
              export PATH="$PWD/scripts:$PATH"
              # HACK: Use custom-built Haskell Language Server (see above).
              export PATH="$HOME/.local/bin:$PATH"
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
