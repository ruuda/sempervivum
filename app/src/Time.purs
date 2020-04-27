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

import Data.Function.Uncurried (Fn2, Fn5, Fn6, runFn2, runFn5, runFn6)
import Effect (Effect)
import Prelude

foreign import data Instant :: Type

foreign import fromGregorianUtcImpl :: Fn6 Int Int Int Int Int Int Instant
foreign import getCurrentInstant :: Effect Instant
foreign import ordInstantImpl :: Fn5 Ordering Ordering Ordering Instant Instant Ordering
foreign import eqInstantImpl :: Fn2 Instant Instant Boolean

instance eqInstant :: Eq Instant where
  eq = runFn2 eqInstantImpl

instance ordInstant :: Ord Instant where
  compare = runFn5 ordInstantImpl LT EQ GT

fromGregorianUtc :: Int -> Int -> Int -> Int -> Int -> Int -> Instant
fromGregorianUtc = runFn6 fromGregorianUtcImpl
