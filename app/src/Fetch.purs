-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Fetch
  ( class HasBody
  , Request
  , Response
  , fetch
  , method
  , readJson
  , url
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut.Core (Json)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import data Request :: Type
foreign import data Response :: Type

foreign import url :: Request -> String
foreign import method :: Request -> String

foreign import fetchImpl :: Request -> Effect (Promise Response)
foreign import readJsonImpl :: forall a. a -> Effect (Promise Json)

fetch :: Request -> Aff Response
fetch request = Promise.toAffE $ fetchImpl request

class HasBody a where
  readJson :: a -> Aff Json

instance requestHasBody :: HasBody Request where
  readJson request = Promise.toAffE $ readJsonImpl request

instance responseHasBody :: HasBody Response where
  readJson response = Promise.toAffE $ readJsonImpl response
