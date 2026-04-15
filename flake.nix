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
        crossPlatforms = p: [ p.musl64 ];
        overlays = [
          haskellNix.overlay
          (final: prev: {
            myHaskellProject =
              final.haskell-nix.hix.project {
                src = ./.;
                compiler-nix-name = "ghc9122";
                evalSystem = "x86_64-linux";
                shell.tools = { cabal = "latest"; haskell-language-server = "latest"; };
                shell.shellHook =
                  let
                    repl-wrapper = final.writeShellScript "repl-wrapper" ''
                      > $HIE_BIOS_OUTPUT
                      for arg in "$@"; do
                          if [[ "$arg" == "--interactive" ]]; then
                              continue
                          elif [[ "$arg" == @* ]]; then
                              grep -v '^--interactive$' "''${arg#@}" >> $HIE_BIOS_OUTPUT
                          else
                              echo "$arg" >> $HIE_BIOS_OUTPUT
                          fi
                      done
                    '';
                    hie-bios = final.writeShellScript "hie-bios" ''
                      cabal repl --builddir $(mktemp -d) --with-repl ${repl-wrapper} Build.hs
                    '';
                    hie-yaml = final.writeText "hie.yaml" ''
                      cradle:
                        multi:
                          - path: Build.hs
                            config:
                              cradle:
                                bios:
                                  program: "${hie-bios}"
                          - path: .
                            config:
                              cradle:
                                cabal:
                    '';
                    # EDIT: hmm, is this why I got loads of orphaned HLSs choking my machine on Thu night?
                    # maybe it was just from the experiments

                    # we have to treat the build script separately
                    # though ideally we could at least still get its deps from Haskell.nix

                    # tbh, HLS should be able to handle this without explicit config anyway
                    # i.e. it should treat scripts separately, based on header, or even just being out of source dirs
                    # also... multi isn't even documented:
                    # https://haskell-language-server.readthedocs.io/en/latest/configuration.html
                    # oh, it is here:
                    # https://github.com/haskell/hie-bios?tab=readme-ov-file#multi-cradle
                    # those should really be in sync
                  in
                  ''
                    ln -sf ${hie-yaml} hie.yaml
                  '';
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
