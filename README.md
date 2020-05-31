# Sempervivum

Sempervivum is a plant watering tracker. It reminds you when to water your
plants based on a per-plant schedule.

## Building

The server written in Haskell and builds with [Stack][stack]. The client is
written in PureScript and builds with [psc-package][psc-package]. You can set up
a local development environment with all dependencies with [Nix][nix].

    # Enter a shell with development dependencies available.
    nix run --command $SHELL

    # Build the client-side code.
    make -C app

    # Build and start the server.
    stack build
    stack exec sempervivum -- serve

## Status and future work

 * **Mostly done**: A server-side rendered app backed by a server side database.
 * **In progress**: A client-side app backed by local storage, able to generate
   notifications.
 * **Future**: Syncing between multiple devices through a server. Data should be
   encrypted client-side, and the user id should be a client-side chosen uuid,
   that can be claimed on a first-come-first-serve basis, so the server cannot
   identify users. All the server needs is a table of `user_id: uuid`,
   `encrypted_data: bytes`, `revision: int`. The data naturally forms a CRDT,
   which facilitates syncing. The user should keep the user id secret to
   prevent others from overwriting the encrypted data. Knowledge of the user id
   authenticates the user.
 * **Far future**: Add usernames (local to the client data), so you can share
   one sync account with everybody in the house or office, and know who watered
   which plant.

## License

 * Sempervivum is free software licensed under the [Apache 2.0][apache2] license.
   Please do not open an issue if you disagree with the choice of license.
 * Sempervivum includes a plant care dataset in the `species` directory. To the
   extent possible under law, the Sempervivum contributors have associated
   [CC0][cc0] with this dataset and waived all copyright and related or
   neighboring rights to this work.

[stack]:       https://docs.haskellstack.org/en/stable/README/
[psc-package]: https://github.com/purescript/psc-package
[nix]:         https://nixos.org/nix/
[apache2]:     https://www.apache.org/licenses/LICENSE-2.0
[cc0]:         https://creativecommons.org/publicdomain/zero/1.0/
