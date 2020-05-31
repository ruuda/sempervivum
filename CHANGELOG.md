# Changelog

## Versioning policy

 * Versions are numbered `major.minor`.
 * Upgrading the server to a new minor version should not involve manual
   intervention.
 * Upgrading the server to a new major version might require manual
   intervention.
 * Once client-side data storage is stable, a new release should be able to load
   client-saved data of the version before it, and possibly of earlier versions.
 * Until then, manual migration will be necessary.

## 1.0

Released 2020-05-31.

Release highlights:

 * This is the initial version.
 * Client-server app, with a single-page client, and a server with an API that
   handles storage, as well as serving static content.
 * Adding new plants requires manual insertion into the database, server side.
