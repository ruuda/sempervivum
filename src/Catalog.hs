-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Catalog
( Catalog
, SpeciesInfo (..)
, readSpeciesOrExit
, speciesText
, lookup
) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Prelude hiding (lookup)
import Toml (TomlCodec, (.=))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.IO as TextIO
import qualified System.Exit as System
import qualified Toml

import Plant (Plant (..), Species (..))

data SpeciesInfo = SpeciesInfo
  { speciesName :: Species
  , speciesWaterDays :: Int
  , speciesFertilizeDays :: Int
  , speciesLight :: Text
  } deriving (Eq, Show)

type Catalog = HashMap Species SpeciesInfo

-- TODO: These newtype wrappers are more annoying than helpful, remove?
speciesText :: Species -> Text
speciesText (Species name) = name

speciesInfoCodec :: TomlCodec SpeciesInfo
speciesInfoCodec = SpeciesInfo
  <$> (Species <$> Toml.text "name" .= speciesText . speciesName)
  <*> Toml.int  "water_days"     .= speciesWaterDays
  <*> Toml.int  "fertilize_days" .= speciesFertilizeDays
  <*> Toml.text "light"          .= speciesLight

speciesListCodec :: TomlCodec [SpeciesInfo]
speciesListCodec = Toml.list speciesInfoCodec "species" .= id

speciesToMap :: [SpeciesInfo] -> Catalog
speciesToMap = HashMap.fromList . fmap f
  where
    f info = (speciesName info, info)

readSpeciesOrExit :: FilePath -> IO Catalog
readSpeciesOrExit fname = do
  catalog <- TextIO.readFile fname
  case Toml.decode speciesListCodec catalog of
    Right species -> pure $ speciesToMap species
    Left msg -> do
      putStrLn $ "Failed to parse " <> fname <> ":"
      TextIO.putStrLn $ Toml.prettyException msg
      System.exitFailure

lookup :: Plant -> Catalog -> Maybe SpeciesInfo
lookup plant = HashMap.lookup (plantSpecies plant)
