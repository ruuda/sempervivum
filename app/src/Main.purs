-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Main where

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Global.Unsafe (unsafeStringify)
import Prelude

import Dom as Dom
import Html as Html
import Plant as Plant
import Species as Species
import Time as Time
import Care as Care
import View as View

main :: Effect Unit
main = launchAff_ $ do
  now <- liftEffect $ Time.getCurrentInstant

  catalog <- Species.getCatalog
  Console.log $ unsafeStringify catalog

  plants <- Plant.getPlants
  Console.log $ unsafeStringify plants

  let matched = Care.match catalog plants
  liftEffect $ Html.withElement Dom.body $ View.renderPlants now matched
