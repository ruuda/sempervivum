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
import Prelude

import Dom as Dom
import Html as Html

main :: Effect Unit
main = launchAff_ $ do
  Console.log "Hello from Purescript."
  liftEffect $ Html.withElement Dom.body $ Html.h2 $ Html.text "Hello"
