{
  description = "Regular Expressions in Haskell";

  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.devshell.url = "github:numtide/devshell";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nix-filter.url = "github:numtide/nix-filter";

  outputs = { self, flake-utils, devshell, nixpkgs, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghc = "ghc8107"; # ghc8107
        overlay = f: p: {
          haskellPackages = f.haskell.packages.${ghc}.override {
            overrides = hf: hp:
              with f.haskell.lib;
              with nix-filter.lib; {
                regex-hs = hf.callCabal2nix "regex-hs" (filter {
                  root = self;
                  exclude = [ (matchExt "cabal") ];
                }) { };
              };
          };
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ devshell.overlay overlay ];
        };
      in {
        devShell = pkgs.devshell.mkShell {
          imports = [ (pkgs.devshell.importTOML ./devshell.toml) ];
          packages = with pkgs; [ haskellPackages.regex-hs ];
        };
      });
}
