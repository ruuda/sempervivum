-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Plant
  ( Plant
  , getPlants
  ) where

import Prelude

import Affjax as Http
import Affjax.ResponseFormat as Http.ResponseFormat
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut.Decode (decodeJson, getField) as Json
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Either (Either (..))
import Effect.Aff (Aff)
import Effect.Exception (Error, error)

import Time (Instant)

newtype Plant = Plant
  { id         :: String
  , species    :: String
  , watered    :: Array Instant
  , fertilized :: Array Instant
  }

instance decodeJsonPlant :: DecodeJson Plant where
  decodeJson json = do
    obj        <- Json.decodeJson json
    id         <- Json.getField obj "id"
    species    <- Json.getField obj "species"
    watered    <- Json.getField obj "watered"
    fertilized <- Json.getField obj "fertilized"
    pure $ Plant { id, species, watered, fertilized }

fatal :: forall m a. MonadThrow Error m => String -> m a
fatal = error >>> throwError

getPlants :: Aff (Array Plant)
getPlants = do
  result <- Http.get Http.ResponseFormat.json "/plants.json"
  case result of
    Left err -> fatal $ "Failed to retrieve plants: " <> Http.printError err
    Right response -> case Json.decodeJson response.body of
      Left err -> fatal $ "Failed to parse plants: " <> err
      Right plants -> pure plants
