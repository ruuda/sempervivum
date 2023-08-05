{
  description = "Sempervivum, a plant watering tracker";

  inputs.nixpkgs.url = "nixpkgs/nixos-23.05";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

      ghc = pkgs.ghc.withPackages (ps: [
        ps.aeson
        ps.tomland
        ps.unordered-containers
        ps.text
      ]);

      buildInputs = [
        # For a self-contained flake we would also need Git and GNU Make,
        # but they are probably already available on the host and the version
        # is not that important, so we'd rather keep the closure size small.

        pkgs.purescript  # Provides the Purescript compiler.
        pkgs.spago       # Provides the Purescript package manager.
        pkgs.esbuild     # Bundler required by Spago.

        # Haskell toolchain for the utility that compiles the catalog.
        ghc

        # For minifying and optimizing the js output of the PureScript compiler.
        pkgs.closurecompiler

        # For compressing plant photos as WebP.
        pkgs.libwebp
      ];
    in
      {
        # Note, we don't package Sempervivum as a Nix package directly, because
        # I don't know how to make psc-package dependency fetching work with
        # Nix.

        devShells."${system}".default = pkgs.mkShell {
          name = "sempervivum";

          nativeBuildInputs = buildInputs;

          # Needed for the locale-archive, to avoid LC_* errors.
          LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
        };
      };
}
