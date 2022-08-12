{
  description = "monpad";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        crossPlatforms = p: [ p.mingwW64 p.musl64 ];
        overlays = [
          haskellNix.overlay
          (final: prev: {
            myHaskellProject =
              final.haskell-nix.hix.project {
                src = ./.;
                compiler-nix-name = "ghc9122";
                evalSystem = "x86_64-linux";
                index-state = "2025-11-03T00:00:00Z";
                shell.tools = { cabal = "latest"; haskell-language-server = "latest"; };
                configureArgs = "-frelease";
                # closurecompiler
                # elmPackages.elm
                # dhall
                inherit crossPlatforms;
              };
          })
          (final: prev: prev.lib.optionalAttrs prev.stdenv.hostPlatform.isMusl {
            libevdev = prev.libevdev.overrideAttrs (_: { dontDisableStatic = true; });
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      in
      pkgs.myHaskellProject.flake { });
}
