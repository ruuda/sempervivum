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
  , seasonalFertilizeDays
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

-- Interpolate between a winter and summer value, depending on the time of the
-- year. These are just some made-up numbers, but that is the case for the
-- values entering into the equation as well. This is no substitute for written
-- instructions, or actually looking at the plant.
interpolateSeason :: Instant -> Duration -> Duration -> Duration
interpolateSeason at winter summer =
  let
    -- The local month depends on the user's time zone, but we only care very
    -- roughly about the time of the year, so an additional 24 hours more or
    -- less is not important.
    amountSummer = case Time.localMonth at of
      1 -> 0.0
      2 -> 0.0
      3 -> 0.5
      4 -> 0.8
      5 -> 1.0
      6 -> 1.0
      7 -> 1.0
      8 -> 1.0
      9 -> 0.8
      10 -> 0.5
      11 -> 0.0
      12 -> 0.0
      _  -> 0.0 -- Impossible
    wsecs = Time.toSeconds winter
    ssecs = Time.toSeconds summer
  in
    Time.fromSeconds $ (1.0 - amountSummer) * wsecs + amountSummer * ssecs

-- Interpolate between the winter and summer watering interval.
seasonalWateringInterval :: Instant -> KnownPlant -> Duration
seasonalWateringInterval at kp =
  let
    Species species = kp.species
  in
    interpolateSeason at
      (Time.fromDays species.waterDaysWinter)
      (Time.fromDays species.waterDaysSummer)

-- Interpolate between the winter and summer fertilization interval.
seasonalFertilizeInterval :: Instant -> KnownPlant -> Duration
seasonalFertilizeInterval at kp =
  let
    Species species = kp.species
  in
    interpolateSeason at
      (Time.fromDays species.fertilizeDaysWinter)
      (Time.fromDays species.fertilizeDaysSummer)

seasonalFertilizeDays :: Instant -> KnownPlant -> Int
seasonalFertilizeDays at kp =
  let
    seconds = Time.toSeconds $ seasonalFertilizeInterval at kp
  in
    Int.ceil $ seconds / (3600.0 * 24.0)

-- See also Plant.adaptiveWateringInterval.
adaptiveWateringInterval :: Instant -> KnownPlant -> Duration
adaptiveWateringInterval now kp =
  let
    -- Prefer to measure at the last watered time, to ensure that the ordering
    -- of when plants need to be watered is stable across different dates.
    at = case Plant.lastWatered kp.plant of
      Nothing -> now
      Just t  -> t
    baseInterval = seasonalWateringInterval at kp
  in
    Plant.adaptiveWateringInterval kp.plant baseInterval

-- Return the watering interval, adapting to previous watering. Returns the
-- result as a range.
adaptiveWaterDaysRange :: Instant -> KnownPlant -> { lower :: Int, upper :: Int }
adaptiveWaterDaysRange at kp =
  let
    seconds = Time.toSeconds $ adaptiveWateringInterval at kp
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
      Just t  -> Time.add (adaptiveWateringInterval now kp) t

sortByNextWater :: Instant -> List KnownPlant -> Array KnownPlant
sortByNextWater now = Array.sortWith (nextWater now) <<< Array.fromFoldable
