-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Species
( Catalog
, Species (..)
, readCatalogOrExit
, lookup
) where

import Data.Either (partitionEithers)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.List (isSuffixOf)
import Prelude hiding (lookup)
import System.FilePath ((</>))
import Toml (TomlCodec, (.=))

import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified System.Exit as System
import qualified System.Directory as Directory
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
      <> "light"                 Aeson..= light species

listToMap :: [Species] -> Catalog
listToMap = HashMap.fromList . fmap (\species -> (name species, species))

-- Return the expected file name of a species, without extension.
slug :: Species -> FilePath
slug species =
  let
    replaceInvalidChars = \case
      c | Char.isAscii c && Char.isAlpha c -> c
      _ -> '_'
  in
    Text.unpack $ Text.map replaceInvalidChars $ Text.toLower $ name species

readSingleEntry :: FilePath -> FilePath -> IO (Either Text Species)
readSingleEntry dirname fname = do
  tomlText <- TextIO.readFile (dirname </> fname)
  case Toml.decode speciesCodec tomlText of
    Right species ->
      if (slug species <> ".toml") == fname
        then pure $ Right species
        else pure $ Left $ mempty
          <> "Error in " <> (Text.pack fname) <> ":\n"
          <> "  File name does not match species '" <> name species <> "'.\n"
          <> "  Expected file to be named '" <> (Text.pack $ slug species) <> ".toml'."
    Left msg ->
      pure $ Left $ mempty
        <> "Failed to parse " <> (Text.pack fname) <> ":\n"
        <> "  " <> Toml.prettyException msg

readCatalog :: FilePath -> IO (Either [Text] Catalog)
readCatalog dirname = do
  fnames <- Directory.listDirectory dirname
  let tomls = filter (".toml" `isSuffixOf`) fnames
  results <- mapM (readSingleEntry dirname) tomls
  case partitionEithers results of
    ([], entries) -> pure $ Right $ listToMap entries
    (errors, _entries) -> pure $ Left errors

readCatalogOrExit :: FilePath -> IO Catalog
readCatalogOrExit dirname = readCatalog dirname >>= \case
  Right catalog -> pure catalog
  Left errors -> do
    TextIO.putStrLn $ Text.intercalate "\n\n" errors
    System.exitFailure

lookup :: SpeciesName -> Catalog -> Maybe Species
lookup = HashMap.lookup
