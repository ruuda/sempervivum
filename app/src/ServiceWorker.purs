-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module ServiceWorker
  ( onInstall
  ) where

import Prelude

import Effect (Effect)
import Effect.Console as Console

onInstall :: Effect Unit
onInstall = Console.log "SW: on install"
