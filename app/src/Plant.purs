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
  , delete
  , hasSpecies
  , insertPlant
  , isDeleted
  , lastFertilized
  , lastWatered
  , newPlant
  , recordFertilized
  , recordWatered
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson, getField, getFieldOptional) as Json
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode (encodeJson) as Json
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty
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
    -- The time at which the plant was deleted. This is a list, so we could
    -- support restore in the future.
  , deleted    :: Array Instant
  }

newtype Plants = Plants (Object Plant)

instance decodeJsonPlant :: DecodeJson Plant where
  decodeJson json = do
    obj        <- Json.decodeJson json
    id         <- Json.getField obj "id"
    species    <- Json.getField obj "species"
    watered    <- Array.sort <$> Json.getField obj "watered"
    fertilized <- Array.sort <$> Json.getField obj "fertilized"
    -- Deletes were added in version 2.7; in case they are not present in the
    -- data, assume an empty list. We could remove this fallback at some point
    -- in the future, when pre-2.7 versions should no longer be in use, but for
    -- now, it's not harmful either.
    deleted <- Json.getFieldOptional obj "deleted" >>= case _ of
      Just deletes -> pure $ Array.sort deletes
      Nothing      -> pure []
    pure $ Plant { id, species, watered, fertilized, deleted }

instance encodeJsonPlant :: EncodeJson Plant where
  encodeJson (Plant plant) =
       "id" := plant.id
    ~> "species" := plant.species
    ~> "watered" := plant.watered
    ~> "fertilized" := plant.fertilized
    ~> "deleted" := plant.deleted
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
    , deleted: []
    }

-- For now, a plant is deleted when it contains a deleted timestamp. In the
-- future, we could support restores, and for that we would interleave deletes
-- and restores, and see whether the most recent event is a restore or delete.
isDeleted :: Plant -> Boolean
isDeleted (Plant p) = not $ Array.null p.deleted

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

delete :: Instant -> Plant -> Plant
delete at (Plant p) =
  Plant $ p { deleted = Array.sort $ Array.snoc p.deleted at }

-- Compute an adaptive watering interval, which is a weighted average of
-- intervals between past waterings, and a base interval that acts as the
-- starting point when there are no past waterings yet.
adaptiveWateringInterval :: Plant -> Duration -> Duration
adaptiveWateringInterval (Plant p) baseInterval =
  case NonEmpty.fromArray p.watered of
    Nothing -> baseInterval
    Just watered ->
      let
        t = NonEmpty.last watered
        -- Weigh watering events with exponential decay with a half-life of 15
        -- days. So the last interval gets weight 1, an interval that ended 7
        -- days before the last one gets weight 0.72, an interval that ended 30
        -- days ago gets weight 0.25. An interval that ended 100 days ago gets
        -- weight 0.01.
        lambda = (Math.log 0.5) / (15.0 * 24.0 * 3600.0)
        weightedDiff t0 t1 =
          let
            weight = Math.exp $ lambda * (Time.toSeconds $ t `Time.subtract` t1)
            seconds = weight * (Time.toSeconds $ t1 `Time.subtract` t0)
          in
            { weight, seconds }
        sumDiff x y = { seconds: x.seconds + y.seconds, weight: x.weight + y.weight }
        -- The base interval weighs in with weight 1.0, so it will have more
        -- relative weight when there is less data, and it will also have more
        -- relative weight when past watering events are longer ago.
        initial = { seconds: Time.toSeconds baseInterval, weight: 1.0 }
        diffs = Array.zipWith weightedDiff p.watered $ NonEmpty.tail watered
        -- We do a left fold so we sum small values first before adding them to
        -- bigger numbers.
        total = Array.foldl sumDiff initial diffs
      in
        Time.fromSeconds $ total.seconds / total.weight

-- Insert the plant, overwiting it if a plant with that id existed already.
insertPlant :: Plant -> Plants -> Plants
insertPlant (Plant p) (Plants ps) = Plants $ Object.insert p.id (Plant p) ps

-- Return wether a non-deleted plant of the given species is part of the collection.
hasSpecies :: String -> Plants -> Boolean
hasSpecies speciesName (Plants ps) =
  let
    isMatch (Plant p) = p.species == speciesName && not isDeleted (Plant p)
  in
    any isMatch ps
