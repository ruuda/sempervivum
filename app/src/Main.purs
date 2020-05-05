-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Main where

import Prelude

import Data.Maybe (Maybe (..))
import Data.String as String
import Data.String.Pattern (Pattern (..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.Object as Object

import Care (MatchedPlants)
import Care as Care
import Dom as Dom
import Html as Html
import Plant (Plant (..), Plants (..))
import Plant as Plant
import Species (Species (..))
import Species as Species
import Time (Instant)
import Time as Time
import View as View

main :: Effect Unit
main = launchAff_ $ do
  pathName <- liftEffect $ Dom.getLocationPathName
  let path = String.split (Pattern "/") (String.drop 1 pathName)
  case path of
    ["app"]              -> runPlantList
    ["app", "plant", id] -> runPlantDetail id
    _                    -> runNotFound

runPlantList :: Aff Unit
runPlantList = do
  now      <- liftEffect $ Time.getCurrentInstant
  catalog  <- Species.getCatalog
  plants   <- Plant.getPlants
  let matched = Care.match catalog plants
  liftEffect $ Html.withElement Dom.body $ View.renderPlants now matched

runPlantDetail :: String -> Aff Unit
runPlantDetail id = do
  now <- liftEffect $ Time.getCurrentInstant
  Plants plants <- Plant.getPlants
  case Object.lookup id plants of
    Nothing ->
      liftEffect $ Html.withElement Dom.body $ Html.p $ Html.text "No plant with that id."

    Just (Plant plant) -> do
      catalog <- Species.getCatalog
      case Object.lookup plant.species catalog of
        Nothing ->
          liftEffect $ Html.withElement Dom.body $ Html.p $ Html.text "Unknown species."

        Just (Species species) ->
          let
            knownPlant = { plant: Plant plant, species: Species species }
          in
            liftEffect $ Html.withElement Dom.body $
              View.renderPlantFull now knownPlant


runNotFound :: Aff Unit
runNotFound =
  liftEffect $ Html.withElement Dom.body $ Html.p $ Html.text "Not found"
