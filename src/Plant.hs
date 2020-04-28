-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Plant
( Plant (..)
, PlantId
, SpeciesName
, lastFertilized
, lastWatered
) where

import Data.Aeson ((.=))
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Prelude hiding (id)

import qualified Data.Aeson as Aeson

type PlantId = Int64
type SpeciesName = Text

data Plant = Plant
  { id         :: PlantId
  , species    :: SpeciesName
  , watered    :: [UTCTime] -- Ordered descending, index 0 is latest.
  , fertilized :: [UTCTime] -- Ordered descending, index 0 is latest.
  } deriving (Eq, Ord, Show)

instance Aeson.ToJSON Plant where
  toJSON = error "Use toEncoding instead."
  toEncoding plant =
    Aeson.pairs $ mempty
      <> "id"         .= show (id plant)
      <> "species"    .= species plant
      <> "watered"    .= reverse (watered plant)
      <> "fertilized" .= reverse (fertilized plant)

lastWatered :: Plant -> Maybe UTCTime
lastWatered = listToMaybe . watered

lastFertilized :: Plant -> Maybe UTCTime
lastFertilized = listToMaybe . fertilized
