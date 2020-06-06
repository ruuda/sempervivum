-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Idb
  ( Db
  , getJson
  , getString
  , open
  , putJson
  , putString
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

foreign import data Db :: Type

foreign import getImpl :: forall a. String -> Db -> EffectFnAff a
foreign import openImpl :: EffectFnAff Db
foreign import putImpl :: forall a. Unit -> String -> a -> Db -> EffectFnAff Unit

open :: Aff Db
open = fromEffectFnAff openImpl

putString :: String -> String -> Db -> Aff Unit
putString key value db = fromEffectFnAff $ putImpl unit key value db

putJson :: String -> Json -> Db -> Aff Unit
putJson key value db = fromEffectFnAff $ putImpl unit key value db

getString :: String -> Db -> Aff String
getString key db = fromEffectFnAff $ getImpl key db

getJson :: String -> Db -> Aff Json
getJson key db = fromEffectFnAff $ getImpl key db
