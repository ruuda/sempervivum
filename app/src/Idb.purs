-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Idb
  ( Db
  , open
  ) where

import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

foreign import data Db :: Type

foreign import openImpl :: EffectFnAff Db

open :: Aff Db
open = fromEffectFnAff openImpl
