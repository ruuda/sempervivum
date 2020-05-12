let
  pkgs = import ./nixpkgs-pinned.nix {};
in
  pkgs.buildEnv {
    name = "sempervivum-devenv";
    paths = [
      pkgs.glibcLocales # Needed for the locale-archive, to avoid LC_* errors.
      pkgs.gmp          # Required by the Haskell runtime.
      pkgs.stack        # Provides the Haskell toolchain.
      pkgs.purescript   # Provides the Purescript compiler.
      pkgs.psc-package  # Provides the Purescript package manager.

      # For minifying and optimizing the js output of the PureScript compiler.
      pkgs.closurecompiler

      # For compressing plant photos as WebP.
      pkgs.libwebp
    ];
  }
