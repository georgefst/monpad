{
  description = "monpad";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        crossPlatforms = p: [ p.mingwW64 p.musl64 ];
        overlays = [
          haskellNix.overlay
          (final: prev: {
            myHaskellProject =
              final.haskell-nix.cabalProject' ({
                src = ./haskell;
                compiler-nix-name = "ghc924";
                shell.tools = { cabal = { }; };
                shell = { inherit crossPlatforms; };
                configureArgs = "-frelease";
              });
          })

          haskellNix.overlay
          (final: prev: prev.lib.optionalAttrs prev.stdenv.hostPlatform.isMusl {
            libevdev = prev.libevdev.overrideAttrs (_: { dontDisableStatic = true; });
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      in
      pkgs.myHaskellProject.flake { inherit crossPlatforms; });
}
