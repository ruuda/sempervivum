-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module AppState where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut.Decode (decodeJson) as Json
import Data.Either (Either (..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)

import Plant (Plants)
import Idb (Db)
import Var (Var)

import Idb as Idb
import Var as Var

type AppState =
  { db :: Db
  , plants :: Var Plants
  }

fatal :: forall m a. MonadThrow Error m => String -> m a
fatal = error >>> throwError

open :: Aff AppState
open = do
  db         <- Idb.open
  plantsJson <- Idb.getJson "plants" db
  plants     <- case Json.decodeJson plantsJson of
    Right ps -> pure ps
    Left err -> fatal $ "Failed to parse plants: " <> err

  var <- liftEffect $ Var.create plants

  pure { db: db, plants: var }
