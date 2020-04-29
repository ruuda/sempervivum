-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module View
  ( renderPlants
  ) where

import Prelude

import Data.List as List

import Care (MatchedPlants)
import Html (Html)
import Html as Html

renderPlants :: MatchedPlants -> Html Unit
renderPlants ps = do
  Html.p $ Html.text $ (show $ List.length ps.knowns) <> " known plants"
  Html.p $ Html.text $ (show $ List.length ps.unknowns) <> " unknown plants"
