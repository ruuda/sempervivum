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
import Control.Monad.Error.Class (catchError, throwError)
import Data.Maybe (Maybe (Just, Nothing))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console

import Cache (Cache)

import Cache as Cache
import Fetch (Request, Response)
import Fetch as Fetch

onInstall :: Aff Unit
onInstall = do
  Console.log "SW: Begin install"
  cache <- Cache.open "v1.0"
  Cache.addAll cache
    [ "/"
    , "/app.js"
    , "/assets/check.svg"
    , "/assets/droplet.svg"
    , "/assets/plant.svg"
    , "/species.json"
    , "/style.css"
    ]
  Console.log "SW: Installation complete"

onActivate :: Aff Unit
onActivate = do
  Console.log "SW: Begin activate"
  -- These is no v0, but if we ever move from v1 to v2, this would be the place
  -- to clean up the v1 cache.
  wasDeleted <- Cache.delete "v0.0"
  Console.log $ "SW: Deleting v0.0 cache returned " <> show wasDeleted
  Console.log "SW: Activation complete"

-- Serve a picture in case it was not found in the cache.
servePicture :: Cache -> Request -> Aff Response
servePicture cache request =
  let
    serveCachedIcon onMissing = do
      cachedFallback <- Cache.matchUrl cache "/assets/plant.svg"
      case cachedFallback of
        Just response -> pure response
        Nothing -> onMissing

    fetchFromNetwork = do
      response <- Fetch.fetch request
      case Fetch.statusCode response of
        200 -> do
          -- If we did not yet have the image but we do now, cache it.
          Cache.put cache request response
          pure response
        404 -> serveCachedIcon $ pure response
        _   -> pure response

    -- In case we could not fetch the picture from the network, for example
    -- because we are offline, or because the server is offline, serve the
    -- generic plant icon instead, from the cache. If that one is not cached,
    -- rethrow the original error.
    serveFallback error = serveCachedIcon $ throwError error
  in do
    Console.log $ "SW: Serving " <> Fetch.url request
    fetchFromNetwork `catchError` serveFallback

startsWith :: String -> String -> Boolean
startsWith prefix str = String.take (String.length prefix) str == prefix

onFetch :: Request -> Aff Response
onFetch request = do
  -- Try to serve from cache first, and if the request is not cached, serve from
  -- the network.
  cache <- Cache.open "v1.0"
  cachedResponse <- Cache.match cache request
  case cachedResponse of
    Nothing ->
      -- If we don't have a cached response, plant pictures get special
      -- behavior, and other requests we try to fetch from the network.
      if startsWith "/images/" $ Fetch.urlPath $ Fetch.url request
        then servePicture cache request
        else Fetch.fetch request
    Just response -> pure response

onInstallPromise :: Effect (Promise Unit)
onInstallPromise = Promise.fromAff onInstall

onActivatePromise :: Effect (Promise Unit)
onActivatePromise = Promise.fromAff onActivate

onFetchPromise :: Request -> Effect (Promise Response)
onFetchPromise request = Promise.fromAff $ onFetch request
