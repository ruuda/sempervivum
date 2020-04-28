-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Time
  ( Instant
  , getCurrentInstant
  , fromGregorianUtc
  ) where

import Data.Argonaut.Core (caseJsonString) as Json
import Data.Either (Either (..))
import Data.Argonaut.Decode (decodeJson) as Json
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Function.Uncurried (Fn2, Fn3, Fn5, Fn6, runFn2, runFn3, runFn5, runFn6)
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Prelude

foreign import data Instant :: Type

foreign import eqInstantImpl :: Fn2 Instant Instant Boolean
foreign import fromGregorianUtcImpl :: Fn6 Int Int Int Int Int Int Instant
foreign import fromIso8601Impl :: Fn3 (Maybe Instant) (Instant -> Maybe Instant) String (Maybe Instant)
foreign import getCurrentInstant :: Effect Instant
foreign import ordInstantImpl :: Fn5 Ordering Ordering Ordering Instant Instant Ordering

instance eqInstant :: Eq Instant where
  eq = runFn2 eqInstantImpl

instance ordInstant :: Ord Instant where
  compare = runFn5 ordInstantImpl LT EQ GT

instance decodeJsonInstant :: DecodeJson Instant where
  decodeJson = Json.caseJsonString
    (Left "Expected Instant to be a string.")
    $ fromIso8601 >>> case _ of
        Nothing -> Left "Failed to parse ISO-8601 string."
        Just t  -> Right t

fromGregorianUtc :: Int -> Int -> Int -> Int -> Int -> Int -> Instant
fromGregorianUtc = runFn6 fromGregorianUtcImpl

fromIso8601 :: String -> Maybe Instant
fromIso8601 = runFn3 fromIso8601Impl Nothing Just
