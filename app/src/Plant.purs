-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Plant
  ( Plant (..)
  , Plants (..)
  , getPlants
  , lastFertilized
  , lastWatered
  , postWatered
  , postWateredFertilized
  ) where

import Prelude

import Affjax as Http
import Affjax.ResponseFormat as Http.ResponseFormat
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson, getField) as Json
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode as Json
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Array as Array
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (Error, error)
import Foreign.Object (Object)
import Foreign.Object as Object

import Time (Instant)
import Util (arrayToMap)

newtype Plant = Plant
  { id         :: String
  , species    :: String
  , watered    :: Array Instant
  , fertilized :: Array Instant
  }

newtype Plants = Plants (Object Plant)

instance decodeJsonPlant :: DecodeJson Plant where
  decodeJson json = do
    obj        <- Json.decodeJson json
    id         <- Json.getField obj "id"
    species    <- Json.getField obj "species"
    watered    <- Array.sort <$> Json.getField obj "watered"
    fertilized <- Array.sort <$> Json.getField obj "fertilized"
    pure $ Plant { id, species, watered, fertilized }

instance encodeJsonPlant :: EncodeJson Plant where
  encodeJson (Plant plant) =
       "id" := plant.id
    ~> "species" := plant.species
    ~> "watered" := plant.watered
    ~> "fertilized" := plant.fertilized
    ~> jsonEmptyObject

instance encodeJsonPlants :: EncodeJson Plants where
  encodeJson (Plants obj) = Json.encodeJson (Object.values obj)

fatal :: forall m a. MonadThrow Error m => String -> m a
fatal = error >>> throwError

getPlants :: Aff Plants
getPlants = do
  result <- Http.get Http.ResponseFormat.json "/plants.json"
  case result of
    Left err -> fatal $ "Failed to retrieve plants: " <> Http.printError err
    Right response -> case Json.decodeJson response.body of
      Left err -> fatal $ "Failed to parse plants: " <> err
      Right plants -> pure $ Plants $ arrayToMap (case _ of Plant p -> p.id) plants

lastWatered :: Plant -> Maybe Instant
lastWatered (Plant p) = Array.last p.watered

lastFertilized :: Plant -> Maybe Instant
lastFertilized (Plant p) = Array.last p.fertilized

recordWatered :: Instant -> Plant -> Plant
recordWatered at (Plant p) =
  Plant $ p { watered = Array.sort $ Array.snoc p.watered at }

recordFertilized :: Instant -> Plant -> Plant
recordFertilized at (Plant p) =
  Plant $ p { fertilized = Array.sort $ Array.snoc p.fertilized at }

postWatered :: Instant -> Plant -> Aff Plant
postWatered now (Plant p) =
  let
    url = "/plants/" <> p.id <> "/watered"
  in do
    result <- Http.post Http.ResponseFormat.ignore url Nothing
    case result of
      Left err -> fatal $ "Failed to post watered: " <> Http.printError err
      Right _  -> do
        liftEffect $ Console.log "Watered posted"
        pure $ recordWatered now (Plant p)

postWateredFertilized :: Instant -> Plant -> Aff Plant
postWateredFertilized now (Plant p) =
  let
    url = "/plants/" <> p.id <> "/watered-fertilized"
  in do
    result <- Http.post Http.ResponseFormat.ignore url Nothing
    case result of
      Left err -> fatal $ "Failed to post watered-fertilized: " <> Http.printError err
      Right _  -> do
        liftEffect $ Console.log "Watered-fertilized posted"
        pure
          $ recordFertilized now
          $ recordWatered now
          $ Plant p
