-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Species
  ( Species (..)
  , getCatalog
  ) where

import Prelude

import Affjax as Http
import Affjax.ResponseFormat as Http.ResponseFormat
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut.Decode (decodeJson, getField) as Json
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Either (Either (..))
import Data.Tuple (Tuple (..))
import Effect.Aff (Aff)
import Effect.Exception (Error, error)
import Foreign.Object (Object)
import Foreign.Object as Object

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

arrayToMap :: Array Species -> Catalog
arrayToMap =
  let
    toTuple (Species s) = Tuple s.name (Species s)
  in
    Object.fromFoldable <<< map toTuple

instance decodeJsonSpecies :: DecodeJson Species where
  decodeJson json = do
    obj                 <- Json.decodeJson json
    name                <- Json.getField obj "name"
    waterDaysSummer     <- Json.getField obj "water_days_summer"
    waterDaysWinter     <- Json.getField obj "water_days_summer"
    waterRemark         <- Json.getField obj "water_remark"
    fertilizeDaysSummer <- Json.getField obj "fertilize_days_summer"
    fertilizeDaysWinter <- Json.getField obj "fertilize_days_summer"
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
  result <- Http.get Http.ResponseFormat.json "/species.json"
  case result of
    Left err -> fatal $ "Failed to retrieve species catalog: " <> Http.printError err
    Right response -> case Json.decodeJson response.body of
      Left err -> fatal $ "Failed to parse species catalog: " <> err
      Right species -> pure $ arrayToMap species
