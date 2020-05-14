-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Main where

import Prelude

import Data.String as String
import Data.String.Pattern (Pattern (..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)

import Care as Care
import Dom as Dom
import Html as Html
import Idb as Idb
import Plant as Plant
import Species as Species
import Time as Time
import View as View

main :: Effect Unit
main = launchAff_ $ do
  now      <- liftEffect $ Time.getCurrentInstant
  catalog  <- Species.getCatalog
  plants   <- Plant.getPlants
  db <- Idb.open
  Idb.put "henk" "steen" db
  let matched = Care.match catalog plants
  liftEffect $ Html.withElement Dom.body $ View.renderPlants now matched
