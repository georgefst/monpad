{
  description = "monpad";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-2511";
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
                shell.tools = { cabal = "latest"; haskell-language-server = "latest"; };
                configureArgs = "-frelease";
                # closurecompiler
                # elmPackages.elm
                # dhall
                # inherit crossPlatforms;
                # TODO apply this only on Windows targets somehow?
                # unclear how to do that here, or correct syntax for putting `configureFlags` in `cabal.project`
                # if possible we should just avoid `basement` due to abandonment and memory safety issues
                # we only rely on it due to TLS stuff in Dhall
                modules = [{ packages.basement.configureFlags = [ "--gcc-option=-Wno-error=int-conversion" ]; }];
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
