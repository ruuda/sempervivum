-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Either (partitionEithers)
import Data.HashMap.Strict (HashMap)
import Data.List (isSuffixOf)
import Data.Text (Text)
import Prelude hiding (lookup)
import System.FilePath ((</>))
import System.IO (stderr)
import Toml (TomlCodec, (.=))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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
readSingleEntry dirname fname =
  let
    decodeBytes tomlBytes = do
      tomlText <- case Text.decodeUtf8' tomlBytes of
        Right txt -> Right txt
        Left exc ->
          Left $ mempty
            <> "Failed to read " <> (Text.pack fname) <> " as UTF-8:\n"
            <> "  " <> (Text.pack $ show exc)

      case Toml.decode speciesCodec tomlText of
        Right species ->
          if (slug species <> ".toml") == fname
            then Right species
            else Left $ mempty
              <> "Error in " <> (Text.pack fname) <> ":\n"
              <> "  File name does not match species '" <> name species <> "'.\n"
              <> "  Expected file to be named '" <> (Text.pack $ slug species) <> ".toml'."
        Left msgs ->
          Left $ mempty
            <> "Failed to parse " <> (Text.pack fname) <> ":\n"
            <> "  " <> Toml.prettyTomlDecodeErrors msgs
  in
    fmap decodeBytes $ ByteString.readFile (dirname </> fname)

readCatalog :: FilePath -> IO (Either [Text] Catalog)
readCatalog dirname = do
  fnames <- Directory.listDirectory dirname
  let tomls = filter (".toml" `isSuffixOf`) fnames
  results <- mapM (readSingleEntry dirname) tomls
  case partitionEithers results of
    ([], entries) -> pure $ Right $ listToMap entries
    (errors, _entries) -> pure $ Left errors

main :: IO ()
main = do
  -- Load the species definitions in the toml files in the "species" directory.
  readCatalog "species" >>= \case
    Right catalog ->
      -- Export the species catalog as json to stdout.
      LazyByteString.putStr $ Aeson.encode $ HashMap.elems catalog
    Left errors -> do
      -- Print errors to stderr, so we can still see them when piping stdout elsewhere.
      TextIO.hPutStrLn stderr $ Text.intercalate "\n\n" errors
      System.exitFailure
