-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
-- Copyright 2019 Ruud van Asseldonk (in Mindec, github.com/ruuda/mindec)
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Var
  ( Var
  , create
  , get
  , set
  ) where

import Effect (Effect)
import Prelude

foreign import data Var :: Type -> Type

foreign import create :: forall a. a -> Effect (Var a)
foreign import get :: forall a. Var a -> Effect a
foreign import set :: forall a. Var a -> a -> Effect Unit
