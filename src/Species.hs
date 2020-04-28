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

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.IO as TextIO
import qualified System.Exit as System
import qualified Toml

type SpeciesName = Text

data Species = Species
  { name                :: SpeciesName
  , waterDaysSummer     :: Int
  , waterDaysWinter     :: Int
  , waterRemark         :: Text
  , fertilizeDaysSummer :: Int
  , fertilizeDaysWinter :: Int
  , fertilizeRemark     :: Text
  , light               :: Text
  } deriving (Eq, Show)

type Catalog = HashMap SpeciesName Species

speciesCodec :: TomlCodec Species
speciesCodec = Species
  <$> Toml.text "name"                  .= name
  <*> Toml.int  "water_days_summer"     .= waterDaysSummer
  <*> Toml.int  "water_days_winter"     .= waterDaysWinter
  <*> Toml.text "water_remark"          .= waterRemark
  <*> Toml.int  "fertilize_days_summer" .= fertilizeDaysSummer
  <*> Toml.int  "fertilize_days_winter" .= fertilizeDaysWinter
  <*> Toml.text "fertilize_remark"      .= fertilizeRemark
  <*> Toml.text "light"                 .= light

catalogCodec :: TomlCodec [Species]
catalogCodec = Toml.list speciesCodec "species" .= id

instance Aeson.ToJSON Species where
  toJSON = error "Use toEncoding instead."
  toEncoding species =
    Aeson.pairs $ mempty
      <> "name"                  Aeson..= name species
      <> "water_days_summer"     Aeson..= waterDaysSummer species
      <> "water_days_winter"     Aeson..= waterDaysWinter species
      <> "water_remark"          Aeson..= waterRemark species
      <> "fertilize_days_summer" Aeson..= fertilizeDaysSummer species
      <> "fertilize_days_winter" Aeson..= fertilizeDaysWinter species
      <> "fertilize_remark"      Aeson..= fertilizeRemark species
      <> "light"                 Aeson..= fertilizeRemark species

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

lookup :: SpeciesName -> Catalog -> Maybe Species
lookup = HashMap.lookup
