-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Cache
  ( Cache
  , addAll
  , delete
  , match
  , open
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Function.Uncurried (Fn3, Fn4, runFn3, runFn4)
import Data.Maybe (Maybe (Just, Nothing))
import Effect (Effect)
import Effect.Aff (Aff)

import Fetch (Request, Response)

foreign import data Cache :: Type

foreign import openImpl :: String -> Effect (Promise Cache)
foreign import deleteImpl :: String -> Effect (Promise Boolean)
foreign import addAllImpl :: Fn3 Cache (Array String) Unit (Effect (Promise Unit))
foreign import matchImpl :: Fn4 Cache Request (Maybe Response) (Response -> Maybe Response) (Effect (Promise (Maybe Response)))

open :: String -> Aff Cache
open name = Promise.toAffE $ openImpl name

delete :: String -> Aff Boolean
delete name = Promise.toAffE $ deleteImpl name

addAll :: Cache -> Array String -> Aff Unit
addAll cache urls = Promise.toAffE $ runFn3 addAllImpl cache urls unit

match :: Cache -> Request -> Aff (Maybe Response)
match cache request = Promise.toAffE $ runFn4 matchImpl cache request Nothing Just