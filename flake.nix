{
  description = "a basic cellular automata using comonads";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
    let 
      pkgs = import nixpkgs {
        overlays = [ (import ./overlay.nix) ];
        inherit system;
      };
    in {
      defaultPackage = pkgs.cellularAutomata;
    }) // {
      overlay = import ./overlay.nix;
    };
}
