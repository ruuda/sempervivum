-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)

import AppState as AppState
import Dom as Dom
import Html as Html
import Species as Species
import Time as Time
import Var as Var
import View as View

main :: Effect Unit
main = launchAff_ $ do
  now      <- liftEffect $ Time.getCurrentInstant
  catalog  <- Species.getCatalog
  appState <- AppState.open catalog
  plants   <- liftEffect $ Var.get appState.plants

  -- Idb.putJson "plants" (Json.encodeJson plants) db
  liftEffect $ Html.withElement Dom.body $ View.renderApp appState now plants
