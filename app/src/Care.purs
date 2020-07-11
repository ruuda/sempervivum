-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Care
  ( KnownPlant
  , MatchedPlants
  , adaptiveWaterDaysRange
  , match
  , nextWater
  , sortByNextWater
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int as Int
import Data.List (List (..), (:))
import Data.Maybe (Maybe (..))
import Foreign.Object as Object

import Plant (Plant (..), Plants (..))
import Plant as Plant
import Species (Catalog, Species (..))
import Time (Duration, Instant)
import Time as Time

type KnownPlant =
  { plant   :: Plant
  , species :: Species
  }

type MatchedPlants =
  { knowns   :: List KnownPlant
  , unknowns :: List Plant
  }

match :: Catalog -> Plants -> MatchedPlants
match catalog (Plants plantMap) =
  let
    plants = Object.values plantMap
    prepend acc (Plant p) = case Object.lookup p.species catalog of
      Just species ->
        let
          known = { plant: Plant p, species: species }
        in
          acc { knowns = known : acc.knowns }

      Nothing ->
        acc { unknowns = (Plant p) : acc.unknowns }
  in
    foldl prepend { knowns: Nil, unknowns: Nil } plants

-- See also Plant.adaptiveWateringInterval.
adaptiveWateringInterval :: KnownPlant -> Duration
adaptiveWateringInterval kp =
  let
    Species species = kp.species
    -- TODO: Adjust to the season.
    baseInterval = Time.fromDays species.waterDaysSummer
  in
    Plant.adaptiveWateringInterval kp.plant baseInterval

adaptiveWaterDaysRange :: KnownPlant -> { lower :: Int, upper :: Int }
adaptiveWaterDaysRange kp =
  let
    seconds = Time.toSeconds $ adaptiveWateringInterval kp
    days = seconds / (3600.0 * 24.0)
    lower = Int.floor days
    preUpper = Int.ceil days
    upper = if lower == preUpper then lower + 1 else preUpper
  in
    { lower, upper }

nextWater :: Instant -> KnownPlant -> Instant
nextWater now kp =
  let
    Species species = kp.species
  in
    case Plant.lastWatered kp.plant of
      Nothing -> now
      Just t  -> Time.add (adaptiveWateringInterval kp) t

sortByNextWater :: Instant -> List KnownPlant -> Array KnownPlant
sortByNextWater now = Array.sortWith (nextWater now) <<< Array.fromFoldable
