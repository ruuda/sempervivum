-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Care
( KnownPlant (..)
, discardEventsAfter
, matchPlants
, nextWater
, wateredRecently
, fertilizedRecently
) where

import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)

import Plant (Plant)
import Species (Catalog, Species)

import qualified Plant
import qualified Species

data KnownPlant = KnownPlant Plant Species

-- Partition plants into a list of known ones matched with their species, and a
-- list of plants whose species is not in the catalog.
matchPlants :: Catalog -> [Plant] -> ([KnownPlant], [Plant])
matchPlants catalog plants =
  let
    go [] result = result
    go (plant : more) (knowns, unknowns) =
      case Species.lookup (Plant.species plant) catalog of
        Just species -> go more (KnownPlant plant species : knowns, unknowns)
        Nothing      -> go more (knowns, plant : unknowns)
  in
    go plants ([], [])

-- Remove all events that occurred after the given time. Used to keep the
-- ordering stable after ticking off a watering task: the plant should not
-- immediately jump to the end of the list.
discardEventsAfter :: UTCTime -> KnownPlant -> KnownPlant
discardEventsAfter discardAfter (KnownPlant plant species) =
  let
    plant' = plant
      { Plant.watered    = filter (< discardAfter) $ Plant.watered plant
      , Plant.fertilized = filter (< discardAfter) $ Plant.fertilized plant
      }
  in
    KnownPlant plant' species

hours :: Int -> NominalDiffTime
hours n = fromIntegral (3600 * n) -- NominalDiffTime converts as seconds.

days :: Int -> NominalDiffTime
days n = hours $ n * 24

-- When should we next water the plant?
nextWater :: UTCTime -> KnownPlant -> UTCTime
nextWater now (KnownPlant plant species) =
  case Plant.lastWatered plant of
    Nothing -> now
    Just t  -> addUTCTime (days $ Species.waterDaysSummer species) t

-- Was the plant watered in the past 8 hours?
wateredRecently :: UTCTime -> Plant -> Bool
wateredRecently now plant =
  case Plant.lastWatered plant of
    Nothing -> False
    Just t  -> t > addUTCTime (hours (-8)) now

-- Was the plant fertilized in the past 8 hours?
fertilizedRecently :: UTCTime -> Plant -> Bool
fertilizedRecently now plant =
  case Plant.lastFertilized plant of
    Nothing -> False
    Just t  -> t > addUTCTime (hours (-8)) now
