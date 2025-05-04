-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module ServiceWorker
  ( onActivatePromise
  , onFetchPromise
  , onInstallPromise
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe (Just, Nothing))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console

import Cache as Cache
import Fetch (Request, Response)
import Fetch as Fetch
import Version as Version

onInstall :: Aff Unit
onInstall = do
  Console.log "SW: Begin install"
  cache <- Cache.open $ "v" <> Version.version
  Cache.addAll cache
    [ "/"
    , "app.js"
    , "assets/check.svg"
    , "assets/droplet.svg"
    , "assets/plant.svg"
    , "manifest.json"
    , "species.json"
    , "style.css"
    ]
  Console.log "SW: Installation complete"

onActivate :: Aff Unit
onActivate = do
  Console.log "SW: Begin activate"
  -- Delete caches of older versions.
  void $ Cache.delete "v2.12"
  void $ Cache.delete "v2.13"
  void $ Cache.delete "v2.14"
  void $ Cache.delete "v2.15"
  Console.log "SW: Activation complete"

onFetch :: Request -> Aff Response
onFetch request = do
  -- Try to serve from cache first, and if the request is not cached, serve from
  -- the network, and then write it to the cache.
  cache <- Cache.open $ "v" <> Version.version
  cachedResponse <- Cache.match cache request
  case cachedResponse of
    Nothing -> do
      response <- Fetch.fetch request
      case Fetch.statusCode response of
        200 -> do
          Cache.put cache request $ Fetch.clone response
          pure response
        -- TODO: To cache or not to cache 404s ...
        404 -> pure response
        _   -> pure response
    Just response -> pure response

onInstallPromise :: Effect (Promise Unit)
onInstallPromise = Promise.fromAff onInstall

onActivatePromise :: Effect (Promise Unit)
onActivatePromise = Promise.fromAff onActivate

onFetchPromise :: Request -> Effect (Promise Response)
onFetchPromise request = Promise.fromAff $ onFetch request
