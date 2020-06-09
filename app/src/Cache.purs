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
  , matchUrl
  , open
  , put
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Function.Uncurried (Fn2, Fn3, Fn4, runFn2, runFn3, runFn4)
import Data.Maybe (Maybe (Just, Nothing))
import Effect (Effect)
import Effect.Aff (Aff)

import Fetch (Request, Response)

foreign import data Cache :: Type

foreign import addAllImpl :: Fn2 Cache (Array String) (Effect (Promise Unit))
foreign import deleteImpl :: String -> Effect (Promise Boolean)
foreign import matchImpl :: forall a. Fn4 Cache a (Maybe Response) (Response -> Maybe Response) (Effect (Promise (Maybe Response)))
foreign import openImpl :: String -> Effect (Promise Cache)
foreign import putImpl :: Fn3 Cache Request Response (Effect (Promise Unit))

open :: String -> Aff Cache
open name = Promise.toAffE $ openImpl name

delete :: String -> Aff Boolean
delete name = Promise.toAffE $ deleteImpl name

addAll :: Cache -> Array String -> Aff Unit
addAll cache urls = Promise.toAffE $ runFn2 addAllImpl cache urls

put :: Cache -> Request -> Response -> Aff Unit
put cache request response = Promise.toAffE $ runFn3 putImpl cache request response

-- JS match accepts both a Request and a url, we expose both as different functions.

match :: Cache -> Request -> Aff (Maybe Response)
match cache request = Promise.toAffE $ runFn4 matchImpl cache request Nothing Just

matchUrl :: Cache -> String -> Aff (Maybe Response)
matchUrl cache url = Promise.toAffE $ runFn4 matchImpl cache url Nothing Just
