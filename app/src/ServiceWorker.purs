-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module ServiceWorker
  ( onInstallPromise
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console

import Cache as Cache

onInstall :: Aff Unit
onInstall = do
  Console.log "SW: Begin install"
  cache <- Cache.open "v1"
  Cache.addAll cache
    [ "/"
    , "/app.js"
    , "/droplet.svg"
    , "/manifest.json"
    , "/plants.json" -- TODO: This will go away when storing locally.
    , "/species.json"
    , "/style.css"
    ]
  Console.log "SW: Installation complete"

onInstallPromise :: Effect (Promise Unit)
onInstallPromise = Promise.fromAff onInstall
