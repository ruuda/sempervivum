-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Plant
  ( Plant (..)
  , Plants (..)
  , adaptiveWateringInterval
  , hasSpecies
  , insertPlant
  , lastFertilized
  , lastWatered
  , newPlant
  , recordWatered
  , recordFertilized
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson, getField) as Json
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode (encodeJson) as Json
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Array as Array
import Data.Foldable (any)
import Data.Maybe (Maybe (Just, Nothing))
import Effect (Effect)
import Effect.Exception (Error, error)
import Foreign.Object (Object)
import Foreign.Object as Object
import Math as Math

import Time as Time
import Time (Duration, Instant)
import Util (arrayToMap, getUniqueId)

newtype Plant = Plant
  { id         :: String
  , species    :: String
  , watered    :: Array Instant
  , fertilized :: Array Instant
  }

newtype Plants = Plants (Object Plant)

instance decodeJsonPlant :: DecodeJson Plant where
  decodeJson json = do
    obj        <- Json.decodeJson json
    id         <- Json.getField obj "id"
    species    <- Json.getField obj "species"
    watered    <- Array.sort <$> Json.getField obj "watered"
    fertilized <- Array.sort <$> Json.getField obj "fertilized"
    pure $ Plant { id, species, watered, fertilized }

instance encodeJsonPlant :: EncodeJson Plant where
  encodeJson (Plant plant) =
       "id" := plant.id
    ~> "species" := plant.species
    ~> "watered" := plant.watered
    ~> "fertilized" := plant.fertilized
    ~> jsonEmptyObject

instance encodeJsonPlants :: EncodeJson Plants where
  encodeJson (Plants obj) = Json.encodeJson (Object.values obj)

instance decodeJsonPlants :: DecodeJson Plants where
  decodeJson json = do
    plants <- Json.decodeJson json
    pure $ Plants $ arrayToMap (case _ of Plant p -> p.id) plants

fatal :: forall m a. MonadThrow Error m => String -> m a
fatal = error >>> throwError

-- Create a plant of the given species, with a new random id.
newPlant :: String -> Effect Plant
newPlant speciesName = do
  id <- getUniqueId
  pure $ Plant
    { id: id
    , species: speciesName
    , watered: []
    , fertilized: []
    }

lastWatered :: Plant -> Maybe Instant
lastWatered (Plant p) = Array.last p.watered

lastFertilized :: Plant -> Maybe Instant
lastFertilized (Plant p) = Array.last p.fertilized

recordWatered :: Instant -> Plant -> Plant
recordWatered at (Plant p) =
  Plant $ p { watered = Array.sort $ Array.snoc p.watered at }

recordFertilized :: Instant -> Plant -> Plant
recordFertilized at (Plant p) =
  Plant $ p { fertilized = Array.sort $ Array.snoc p.fertilized at }

-- Compute an adaptive watering interval, which is a weighted average of
-- intervals between past waterings, and a base interval that acts as the
-- starting point when there are no past waterings yet.
adaptiveWateringInterval :: Plant -> Duration -> Duration
adaptiveWateringInterval (Plant p) baseInterval =
  case Array.unsnoc p.watered of
    Nothing -> baseInterval
    Just { init: tailWatered, last: t } ->
      let
        -- Weigh watering events with exponential decay with a half-life of 30
        -- days. So the last interval gets weight 1, an interval that ended 15
        -- days before the last one gets weight 0.707, an interval that ended 30
        -- days ago gets weight 0.5. An interval that ended 180 days ago gets
        -- weight 0.015.
        lambda = (Math.log 0.5) / (30.0 * 24.0 * 3600.0)
        weightedDiff t0 t1 =
          let
            weight = Math.exp $ lambda * (Time.toSeconds $ t1 `Time.subtract` t)
            seconds = weight * (Time.toSeconds $ t1 `Time.subtract` t0)
          in
            { weight, seconds }
        sumDiff x y = { seconds: x.seconds + y.seconds, weight: x.weight + y.weight }
        -- The base interval weighs in with weight 1.0, so it will have more
        -- relative weight when there is less data, and it will also have more
        -- relative weight when past watering events are longer ago.
        initial = { seconds: Time.toSeconds baseInterval, weight: 1.0 }
        diffs = Array.zipWith weightedDiff p.watered tailWatered
        total = Array.foldl sumDiff zero diffs
      in
        Time.fromSeconds $ total.seconds / total.weight

-- Insert the plant, overwiting it if a plant with that id existed already.
insertPlant :: Plant -> Plants -> Plants
insertPlant (Plant p) (Plants ps) = Plants $ Object.insert p.id (Plant p) ps

-- Return wether a plant of the given species is part of the collection.
hasSpecies :: String -> Plants -> Boolean
hasSpecies speciesName (Plants ps) = any (case _ of Plant p -> p.species == speciesName) ps
