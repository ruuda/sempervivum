-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Types
( PlantId (..)
, Plant (..)
, Species (..)
) where

import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import Data.Text (Text)

newtype PlantId = PlantId Int64 deriving (Eq, Ord, Show)
newtype Species = Species Text deriving (Eq, Ord, Show)

data Plant = Plant
  { plantId             :: PlantId
  , plantSpecies        :: Species
  , plantLastWatered    :: Maybe UTCTime
  , plantLastFertilized :: Maybe UTCTime
  } deriving (Eq, Ord, Show)
