-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Species
  ( Catalog
  , Species (..)
  , getCatalog
  , search
  ) where

import Prelude

import Affjax as Http
import Affjax.ResponseFormat as Http.ResponseFormat
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut.Decode (decodeJson, getField) as Json
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either (..))
import Data.String as String
import Data.String.Pattern (Pattern (..))
import Effect.Aff (Aff)
import Effect.Exception (Error, error)
import Foreign.Object (Object)
import Foreign.Object as Object

import Util (arrayToMap)

newtype Species = Species
  { name                :: String
  , waterDaysSummer     :: Int
  , waterDaysWinter     :: Int
  , waterRemark         :: String
  , fertilizeDaysSummer :: Int
  , fertilizeDaysWinter :: Int
  , fertilizeRemark     :: String
  , light               :: String
  }

type Catalog = Object Species

instance decodeJsonSpecies :: DecodeJson Species where
  decodeJson json = do
    obj                 <- Json.decodeJson json
    name                <- Json.getField obj "name"
    waterDaysSummer     <- Json.getField obj "water_days_summer"
    waterDaysWinter     <- Json.getField obj "water_days_winter"
    waterRemark         <- Json.getField obj "water_remark"
    fertilizeDaysSummer <- Json.getField obj "fertilize_days_summer"
    fertilizeDaysWinter <- Json.getField obj "fertilize_days_winter"
    fertilizeRemark     <- Json.getField obj "fertilize_remark"
    light               <- Json.getField obj "light"
    pure $ Species
      { name
      , waterDaysSummer
      , waterDaysWinter
      , waterRemark
      , fertilizeDaysSummer
      , fertilizeDaysWinter
      , fertilizeRemark
      , light
      }

fatal :: forall m a. MonadThrow Error m => String -> m a
fatal = error >>> throwError

getCatalog :: Aff Catalog
getCatalog = do
  result <- Http.get Http.ResponseFormat.json "species.json"
  case result of
    Left err -> fatal $ "Failed to retrieve species catalog: " <> Http.printError err
    Right response -> case Json.decodeJson response.body of
      Left err -> fatal $ "Failed to parse species catalog: " <> printJsonDecodeError err
      Right species -> pure $ arrayToMap (case _ of Species s -> s.name) species

-- Search for a species by name.
search :: String -> Catalog -> Array Species
search needle catalog =
  let
    -- We search on lowercased species names, so we have a case-insensitive
    -- search.
    pattern = Pattern $ String.toLower needle
    isMatch = String.contains pattern <<< String.toLower
    matches = Object.values $ Object.filterKeys isMatch catalog
  in
    -- Sort matches by the index of the needle, so prefix matches are first.
    Array.sortWith (case _ of Species s -> String.indexOf pattern $ String.toLower s.name) matches
