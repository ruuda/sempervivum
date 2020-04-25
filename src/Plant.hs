-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Plant
( PlantId (..)
, Plant (..)
, Species (..)
, plantLastWatered
, plantLastFertilized
) where

import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import Data.Maybe (listToMaybe)

newtype PlantId = PlantId Int64 deriving (Eq, Ord, Show, Hashable)
newtype Species = Species Text deriving (Eq, Ord, Show, Hashable)

data Plant = Plant
  { plantId         :: PlantId
  , plantSpecies    :: Species
  , plantWatered    :: [UTCTime] -- Ordered descending, index 0 is latest.
  , plantFertilized :: [UTCTime] -- Ordered descending, index 0 is latest.
  } deriving (Eq, Ord, Show)

plantLastWatered :: Plant -> Maybe UTCTime
plantLastWatered = listToMaybe . plantWatered

plantLastFertilized :: Plant -> Maybe UTCTime
plantLastFertilized = listToMaybe . plantFertilized
