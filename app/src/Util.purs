-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Util
 ( getRandomBytes
 , getUniqueId
 ) where

import Data.Foldable (intercalate)
import Data.Int as Int
import Effect (Effect)
import Prelude

foreign import getRandomBytes :: Int -> Effect (Array Int)

-- Generate a random 64-bit id, encode it as hexadecimal string.
getUniqueId :: Effect String
getUniqueId =
  let
    hex x = (if x < 16 then "0" else "") <> (Int.toStringAs Int.hexadecimal x)
  in
    -- 64 bits is plenty for tracking your plants. It does not have to be
    -- globally unique, only unique per user.
    map (intercalate "" <<< map hex) (getRandomBytes 8)

