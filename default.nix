let
  pkgs = import ./nixpkgs-pinned.nix {};
in
  pkgs.buildEnv {
    name = "sempervivum-devenv";
    paths = [
      pkgs.glibcLocales # Needed for the locale-archive, to avoid LC_* errors.
      pkgs.gmp          # Required by the Haskell runtime.
      pkgs.stack        # Provides the Haskell toolchain.
    ];
  }
