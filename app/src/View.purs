-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module View
  ( renderPlants
  , renderPlantFull
  ) where

import Prelude

import Control.Monad.Reader.Class (ask)
import Data.Foldable (traverse_)
import Data.List (List (..))
import Data.List as List
import Data.Maybe (Maybe (..))
import Data.String.Common as String
import Data.String.Pattern (Pattern (..), Replacement (..))
import Data.Time.Duration (Milliseconds (..))
import Effect.Aff as Aff
import Effect.Class (liftEffect)

import Care (MatchedPlants, KnownPlant)
import Care as Care
import Html (Html)
import Html as Html
import Plant (Plant (..))
import Plant as Plant
import Species (Species (..))
import Time (Instant)
import Time as Time

-- Return (t2 - t1) in number of local days. This considers only the local date,
-- and ignores the time of the day. So if t1 is 5 seconds before midnight in
-- its local timezone, and t2 is 10 seconds after t1, then the difference
-- would be 1 day. If both were a minute earlier, the difference would be 0
-- days.
relativeDate :: Instant -> Instant -> Int
relativeDate t1 t2 = (Time.localJulianDay t2) - (Time.localJulianDay t1)

lastWatered :: Instant -> Plant -> String
lastWatered now plant = case Plant.lastWatered plant of
  Nothing -> "Never watered before"
  Just t -> case relativeDate now t of
    -1        -> "Watered yesterday"
    0         -> "Watered today"
    n | n < 0 -> "Watered " <> show (-n) <> " days ago"
    _         -> "Watered in the future"

lastFertilized :: Instant -> Plant -> String
lastFertilized now plant = case Plant.lastFertilized plant of
  Nothing -> "Never fertilized before"
  Just t -> case relativeDate now t of
    -1        -> "Fertilized yesterday"
    0         -> "Fertilized today"
    n | n < 0 -> "Fertilized " <> show (-n) <> " days ago"
    _         -> "Fertilized in the future"

nextWater :: Instant -> KnownPlant -> String
nextWater now plant = case relativeDate now (Care.nextWater now plant) of
  -1        -> "Needs water since yesterday"
  0         -> "Needs water today"
  1         -> "Needs water tomorrow"
  n | n < 0 -> "Needs water since " <> show (-n) <> " days"
  n         -> "Water in " <> show n <> " days"

speciesImageUrl :: Species -> String
speciesImageUrl (Species species) =
  let
    slug
      = String.replaceAll (Pattern " ") (Replacement "_")
      $ String.replaceAll (Pattern "-") (Replacement "_")
      $ String.toLower
      $ species.name
  in
    "/" <> slug <> ".webp"

renderPlants :: Instant -> MatchedPlants -> Html Unit
renderPlants now ps = do
  Html.h1 $ Html.text "Plants"
  traverse_ (renderPlantItem now) (Care.sortByNextWater now ps.knowns)
  case ps.unknowns of
    Nil -> pure unit
    xs  -> Html.p $ Html.text $ "And " <> (show $ List.length ps.unknowns) <> " unknown plants"

renderPlantItem :: Instant -> KnownPlant -> Html Unit
renderPlantItem now knownPlant =
  let
    Plant plant = knownPlant.plant
    Species species = knownPlant.species
    _href = "/app/plant/" <> plant.id
  in
    Html.div $ do
      outer <- ask

      Html.div $ do
        Html.setId plant.id
        Html.addClass "plant"
        Html.img (speciesImageUrl knownPlant.species) species.name (pure unit)
        Html.h2 $ Html.text plant.species
        Html.p $ Html.text $ nextWater now knownPlant
        Html.onClick $ Aff.launchAff_ $ do
          -- We need to add these classes with a delay in between; when they get
          -- added simultaneously, the css transition does not take effect.
          liftEffect $ Html.withElement outer $ Html.addClass "expanded"
          Aff.delay (Milliseconds 1.0)
          liftEffect $ Html.withElement outer $ Html.addClass "unveiled"

      Html.div $ do
        Html.addClass "plant-details"
        Html.p $ do
          Html.addClass "multi"
          Html.text species.waterRemark
        Html.p $ do
          Html.addClass "multi"
          Html.text species.fertilizeRemark
        Html.p $ Html.text $ "Needs water every " <> (show species.waterDaysSummer) <> " days."
        Html.p $ Html.text $ lastWatered now (Plant plant)
        Html.p $ Html.text $ lastFertilized now (Plant plant)
        Html.button $ do
          Html.text "watered"
        Html.button $ do
          Html.text "watered + fertilized"

renderPlantFull :: Instant -> KnownPlant -> Html Unit
renderPlantFull now knownPlant =
  let
    Plant plant = knownPlant.plant
    Species species = knownPlant.species
  in
    Html.div $ do
      Html.setId plant.id
      Html.addClass "plant"
      Html.h1 $ Html.text plant.species
      Html.p $ do
        Html.addClass "multi"
        Html.text $ nextWater now knownPlant
      Html.p $ do
        Html.addClass "multi"
        Html.text species.waterRemark
      Html.p $ do
        Html.addClass "multi"
        Html.text species.fertilizeRemark
      Html.p $ Html.text $ "Needs water every " <> (show species.waterDaysSummer) <> " days."
      Html.p $ Html.text $ lastWatered now (Plant plant)
      Html.p $ Html.text $ lastFertilized now (Plant plant)
      Html.button $ do
        Html.img "/watered.svg" "watered" (pure unit)
        Html.text "watered"
      Html.button $ do
        Html.img "/fertilized.svg" "watered and fertilized" (pure unit)
        Html.text "watered + fertilized"
