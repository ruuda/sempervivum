# Changelog

## Versioning policy

 * Versions are numbered `major.minor`.
 * New versions should be able to read the local data stored by the prior version.
 * Deploying a new minor version should not involve manual intervention apart
   from copying the new files. Deploying a new major version may require manual
   intervention.

## 2.1

Released 2020-07-07.

 * Make the app a bit friendlier for first use.
 * Internal cleanup, release automation through GitHub actions.

## 2.0

Released 2020-07-07.

 * This is the first version that can be deployed as a static site.
 * To upgrade, download `/plants.json` from the old server, and use the new
   “restore” button in the application to import it to local storage.

## 1.0

Released 2020-05-31.

Release highlights:

 * This is the initial version.
 * Client-server app, with a single-page client, and a server with an API that
   handles storage, as well as serving static content.
 * Adding new plants requires manual insertion into the database, server side.
