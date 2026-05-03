{
  inputs.nixpkgs.url = "__NIXPKGS_REF__";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages.default = import ./default.nix { inherit pkgs; };
      }
    );
}
