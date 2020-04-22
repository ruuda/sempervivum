-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Catalog
( SpeciesInfo (..)
, readSpeciesOrExit
) where

import Data.Text (Text)
import Toml (TomlCodec, (.=))

import qualified Data.Text.IO as TextIO
import qualified System.Exit as System
import qualified Toml

import Types (Species (..))

data SpeciesInfo = SpeciesInfo
  { speciesName :: Species
  , speciesWaterDays :: Int
  , speciesFertilizeDays :: Int
  , speciesDescription :: Text
  } deriving (Eq, Show)

speciesText :: Species -> Text
speciesText (Species name) = name

speciesInfoCodec :: TomlCodec SpeciesInfo
speciesInfoCodec = SpeciesInfo
  <$> (Species <$> Toml.text "name" .= speciesText . speciesName)
  <*> Toml.int  "water_days"     .= speciesWaterDays
  <*> Toml.int  "fertilize_days" .= speciesFertilizeDays
  <*> Toml.text "description"    .= speciesDescription

speciesListCodec :: TomlCodec [SpeciesInfo]
speciesListCodec = Toml.list speciesInfoCodec "species" .= id

readSpeciesOrExit :: FilePath -> IO [SpeciesInfo]
readSpeciesOrExit fname = do
  catalog <- TextIO.readFile fname
  case Toml.decode speciesListCodec catalog of
    Right species -> pure species
    Left msg -> do
      putStrLn $ "Failed to parse " <> fname <> ":"
      TextIO.putStrLn $ Toml.prettyException msg
      System.exitFailure
