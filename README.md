# Sempervivum

Sempervivum is a plant watering tracker. It reminds you when to water your
plants based on a per-plant schedule.

Sempervivum consists of a single-page web application that can be hosted as a
static site. It stores all user data locally, on the user’s device. The current
version is hosted at https://ruuda.github.io/sempervivum/.

## Building

The client is written in PureScript and builds with [psc-package][psc-package].
[Closure Compiler][closure] takes care of bundling and minification. There is
also a Haskell program that bundles the species catalog into a json file. It
builds with GHC, and requires a few Hakell packages to be available. You can set
up a local development environment with all dependencies with [Nix][nix].

    # Enter a shell with development dependencies available.
    nix run --command $SHELL

    # Build the app.
    make -j4

    # Serve the app.
    python -m http.server --directory out

## Status and future work

 * A basic version is available at https://ruuda.github.io/sempervivum/.
 * Currently Sempervivum can be deployed as a static site.
 * In the future I would like to add a server that facilitates syncing, so you
   can use the app from multiple devices, or maybe with multiple users. Data
   would be encrypted client-side.

## License

 * Sempervivum is free software licensed under the [Apache 2.0][apache2] license.
   Please do not open an issue if you disagree with the choice of license.
 * Sempervivum includes a plant care dataset in the `species` directory. To the
   extent possible under law, the Sempervivum contributors have associated
   [CC0][cc0] with this dataset and waived all copyright and related or
   neighboring rights to this work.

[closure]:     https://github.com/google/closure-compiler
[psc-package]: https://github.com/purescript/psc-package
[nix]:         https://nixos.org/nix/
[apache2]:     https://www.apache.org/licenses/LICENSE-2.0
[cc0]:         https://creativecommons.org/publicdomain/zero/1.0/
