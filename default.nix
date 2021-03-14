let
  pkgs = import ./nixpkgs-pinned.nix {};

  # We take Haskell dependencies from Nix as well, my Stack + Makefile approach
  # complains about missing files and I don't want to spend the energy
  # debugging.
  ghc = pkgs.haskellPackages.ghcWithPackages (p: [
    p.aeson
    p.tomland
    p.unordered-containers
    p.text
  ]);
in
  pkgs.buildEnv {
    name = "sempervivum-devenv";
    paths = [
      pkgs.glibcLocales # Needed for the locale-archive, to avoid LC_* errors.
      pkgs.purescript   # Provides the Purescript compiler.
      pkgs.psc-package  # Provides the Purescript package manager.

      # Haskell toolchain for the utility that compiles the catalog.
      ghc

      # For minifying and optimizing the js output of the PureScript compiler.
      pkgs.closurecompiler

      # For compressing plant photos as WebP.
      pkgs.libwebp
    ];
  }
