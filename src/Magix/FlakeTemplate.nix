{
  inputs.nixpkgs.url = "__NIXPKGS_REF__";

  outputs =
    { nixpkgs, ... }:
    {
      packages = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = import ./default.nix { inherit pkgs; };
        }
      );
    };
}
