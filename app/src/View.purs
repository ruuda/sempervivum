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

import Data.Foldable (traverse_)
import Data.List (List (..))
import Data.List as List

import Care (MatchedPlants, KnownPlant)
import Html (Html)
import Html as Html
import Plant (Plant (..))
import Species (Species (..))

renderPlants :: MatchedPlants -> Html Unit
renderPlants ps = do
  Html.h1 $ Html.text "Plants"
  traverse_ renderPlant ps.knowns
  case ps.unknowns of
    Nil -> pure unit
    xs  -> Html.p $ Html.text $ "And " <> (show $ List.length ps.unknowns) <> " unknown plants"

renderPlant :: KnownPlant -> Html Unit
renderPlant knownPlant =
  let
    Plant plant = knownPlant.plant
    Species species = knownPlant.species
  in
    Html.div $ do
      Html.setId plant.id
      Html.addClass "plant"
      Html.h2 $ Html.text plant.species
      Html.p $ Html.text "Needs water at some point."
      Html.p $ Html.text "Maybe watered previously."
      Html.p $ Html.text "Maybe fertilized previously."
      Html.p $ Html.text $ "Needs water every " <> (show species.waterDaysSummer) <> " days."
      Html.p $ Html.text species.waterRemark
      Html.p $ Html.text species.fertilizeRemark
