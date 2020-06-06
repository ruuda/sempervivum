-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module AppState
  ( open
  , postWatered
  , postWateredFertilized
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut.Decode (decodeJson) as Json
import Data.Argonaut.Encode (encodeJson) as Json
import Data.Either (Either (..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)

import Plant (Plants, Plant)
import Idb (Db)
import Var (Var)

import Idb as Idb
import Plant as Plant
import Time as Time
import Var as Var

type AppState =
  { db :: Db
  , plants :: Var Plants
  }

fatal :: forall m a. MonadThrow Error m => String -> m a
fatal = error >>> throwError

open :: Aff AppState
open = do
  db         <- Idb.open
  plantsJson <- Idb.getJson "plants" db
  plants     <- case Json.decodeJson plantsJson of
    Right ps -> pure ps
    Left err -> fatal $ "Failed to parse plants: " <> err

  var <- liftEffect $ Var.create plants

  pure { db: db, plants: var }

-- Modify the plant, replace the plant in the app state, return the new plant.
modifyPlant :: (Plant -> Aff Plant) -> AppState -> Plant -> Aff Plant
modifyPlant f appState plant = do
  newPlant <- f plant
  -- Update the deserialized plant list in the mutable volatile app state.
  -- We don't reload the plant list from IndexedDB, so if you open two tabs,
  -- you may lose data. I consider that acceptable, it is easier than
  -- integrating IndexedDB operations deeply with PureScript types.
  plants   <- liftEffect $ Var.get appState.plants
  liftEffect $ Var.set appState.plants $ Plant.insertPlant newPlant plants
  -- Also persist the new plant list in IndexedDB.
  Idb.putJson "plants" (Json.encodeJson plants) appState.db
  pure newPlant

-- Record a watered event for the given plant at the current time, then replace
-- the plant in the app state, return the new plant.
postWatered :: AppState -> Plant -> Aff Plant
postWatered = modifyPlant $ \plant -> do
  now <- liftEffect Time.getCurrentInstant
  pure $ Plant.recordWatered now plant

-- Record a watered and fertilized event for the given plant at the current time,
-- then replace the plant in the app state, return the new plant.
postWateredFertilized :: AppState -> Plant -> Aff Plant
postWateredFertilized = modifyPlant $ \plant -> do
  now <- liftEffect Time.getCurrentInstant
  pure $ Plant.recordFertilized now $ Plant.recordWatered now plant
