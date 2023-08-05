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

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson, getField, getFieldOptional) as Json
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode (encodeJson) as Json
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty
import Data.Foldable (any, sum)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Number as Number
import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as Object

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

newtype Delta = Delta
  { seconds :: Number
  , weight :: Number
  }

derive instance eqDelta :: Eq Delta
derive instance ordDelta :: Ord Delta

-- Compute an adaptive watering interval, which is a weighted quantile of
-- intervals between past waterings, and a base interval that acts as the
-- starting point when there are no past waterings yet.
adaptiveWateringInterval :: Number -> Plant -> Duration -> Duration
adaptiveWateringInterval quantile (Plant p) baseInterval =
  -- We consider only the past 50 watering events, mostly because it makes debug
  -- prints easier to read. Waterings longer ago will have a negligible weight
  -- anyway.
  case NonEmpty.fromArray $ Array.takeEnd 50 p.watered of
    Nothing -> baseInterval
    Just watered ->
      let
        t = NonEmpty.last watered
        -- Weigh watering events with exponential decay with a half-life of 20
        -- days. So the last interval gets weight 1, an interval that ended 7
        -- days before the last one gets weight 0.78, an interval that ended 30
        -- days ago gets weight 0.35. An interval that ended 100 days ago gets
        -- weight 0.03.
        lambda = (Number.log 0.5) / (20.0 * 24.0 * 3600.0)
        weightedDiff t0 t1 =
          let
            weight = Number.exp $ lambda * (Time.toSeconds $ t `Time.subtract` t1)
            seconds = Time.toSeconds $ t1 `Time.subtract` t0
          in
            Delta { seconds, weight }
        -- The base interval weighs in with weight 1.2, so it will have more
        -- relative weight when there is less data, and it will also have more
        -- relative weight when past watering events are longer ago. We pick
        -- 1.2, because for plants that are watered infrequently, once the base
        -- interval goes down (because it’s starting to be summer), if the past
        -- few intervals overshoot the target by a lot, the target will not
        -- affect the median at all, so give it a bit of an edge to improve the
        -- odds for that.
        diffs = Array.snoc
          (Array.zipWith weightedDiff
            (NonEmpty.toArray watered)
            (NonEmpty.tail watered)
          )
          (Delta { seconds: Time.toSeconds baseInterval, weight: 1.2 })
        -- We do a left fold so we sum small values first before adding them to
        -- bigger numbers.
        totalWeight = sum $ map (\(Delta d) -> d.weight) diffs
        -- For the watering interval, we take the weighted median. In the past
        -- we took a weighted average, but I tend to overshoot my target more
        -- often than I undershoot it. (E.g. I'm away for a weekend, and I water
        -- the plants one or two days later.) In a sense, the algorithm can't
        -- distinguish between intentionally watering late because the plant
        -- needs less water, vs. accidentally watering late. The median is more
        -- robust against outliers, so being a few days late once will not
        -- affect the scheduled interval as much.
        sortedDiffs = Array.sort diffs
        step acc xs = case Array.uncons xs of
          Nothing -> baseInterval
          Just { head: Delta head, tail: _ } | acc.weight + head.weight > quantile * totalWeight ->
            let
              z = (quantile * totalWeight - acc.weight) / head.weight
            in
              Time.fromSeconds $ (1.0 - z) * acc.seconds + z * head.seconds
          Just { head: Delta head, tail } ->
            step
              { weight: acc.weight + head.weight, seconds: head.seconds }
              tail
      in
        step
          { weight: 0.0, seconds: Time.toSeconds baseInterval }
          sortedDiffs

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
