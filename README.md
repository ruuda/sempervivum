# Sempervivum

Sempervivum is a plant watering tracker. It reminds you when to water your
plants based on a per-plant schedule.

## Future work

 * **v1**: A server-side rendered app backed by a server side database.
 * **v2**: A client-side app backed by local storage, able to generate
   notifications.
 * **v3**: Syncing between multiple devices through a server. Data should be
   encrypted client-side, and the username must be hashed, so the server cannot
   identify users. All the server needs is a table of `username_hash: bytes`,
   `encrypted_data: bytes`, `revision: int`. The data naturally forms a CRDT,
   which facilitates syncing.
 * **v4**: Decoupling usernames from sync, so you can share one sync accout with
   everybody in the house or office, and know who watered which plant.

## License

 * Sempervivum is free software licensed under the [Apache 2.0][apache2] license.
   Please do not open an issue if you disagree with the choice of license.
 * Sempervivum includes a plant care dataset in the `species` directory. To the
   extent possible under law, the Sempervivum contributors have associated
   [CC0][cc0] with this dataset and waived all copyright and related or
   neighboring rights to this work.

[apache2]:    https://www.apache.org/licenses/LICENSE-2.0
[cc0]:        https://creativecommons.org/publicdomain/zero/1.0/
