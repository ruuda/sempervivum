-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Main where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut.Decode (decodeJson) as Json
import Data.Either (Either (..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)

import Dom as Dom
import Html as Html
import Idb as Idb
import Species as Species
import Time as Time
import View as View

fatal :: forall m a. MonadThrow Error m => String -> m a
fatal = error >>> throwError

main :: Effect Unit
main = launchAff_ $ do
  now        <- liftEffect $ Time.getCurrentInstant
  catalog    <- Species.getCatalog
  db         <- Idb.open
  plantsJson <- Idb.getJson "plants" db
  plants     <- case Json.decodeJson plantsJson of
    Right ps -> pure ps
    Left err -> fatal $ "Failed to parse plants: " <> err
  -- Idb.putJson "plants" (Json.encodeJson plants) db
  liftEffect $ Html.withElement Dom.body $ View.renderApp now catalog plants
