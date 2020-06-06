-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Blob
  ( Blob
  , toBlob
  , getObjectUrl
  , revokeObjectUrl
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Effect (Effect)

foreign import data Blob :: Type

foreign import toBlob :: Json -> Blob
foreign import getObjectUrl :: Blob -> Effect String
foreign import revokeObjectUrl :: String -> Effect Unit
