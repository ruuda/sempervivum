-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Species
( Catalog
, Species (..)
, readCatalogOrExit
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

import Plant (Plant (..))

type SpeciesName = Text

data Species = Species
  { name          :: SpeciesName
  , waterDays     :: Int
  , fertilizeDays :: Int
  , light         :: Text
  } deriving (Eq, Show)

type Catalog = HashMap SpeciesName Species

speciesCodec :: TomlCodec Species
speciesCodec = Species
  <$> Toml.text "name"           .= name
  <*> Toml.int  "water_days"     .= waterDays
  <*> Toml.int  "fertilize_days" .= fertilizeDays
  <*> Toml.text "light"          .= light

catalogCodec :: TomlCodec [Species]
catalogCodec = Toml.list speciesCodec "species" .= id

listToMap :: [Species] -> Catalog
listToMap = HashMap.fromList . fmap (\species -> (name species, species))

readCatalogOrExit :: FilePath -> IO Catalog
readCatalogOrExit fname = do
  catalog <- TextIO.readFile fname
  case Toml.decode catalogCodec catalog of
    Right species -> pure $ listToMap species
    Left msg -> do
      putStrLn $ "Failed to parse " <> fname <> ":"
      TextIO.putStrLn $ Toml.prettyException msg
      System.exitFailure

lookup :: Plant -> Catalog -> Maybe Species
lookup plant = HashMap.lookup (plantSpecies plant)
