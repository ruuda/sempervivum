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

import Control.Monad.Reader.Class (ask, local)
import Data.Foldable (traverse_)
import Data.Int as Int
import Data.List (List (..))
import Data.List as List
import Data.Maybe (Maybe (..))
import Data.String.Common as String
import Data.String.Pattern (Pattern (..), Replacement (..))
import Data.Time.Duration (Milliseconds (..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Math (sqrt)

import Care (MatchedPlants, KnownPlant)
import Care as Care
import Dom (Element)
import Html (Html)
import Html as Html
import Plant (Plant (..))
import Plant as Plant
import Species (Species (..))
import Time (Instant)
import Time as Time
import Var as Var

-- Return (t2 - t1) in number of local days. This considers only the local date,
-- and ignores the time of the day. So if t1 is 5 seconds before midnight in
-- its local timezone, and t2 is 10 seconds after t1, then the difference
-- would be 1 day. If both were a minute earlier, the difference would be 0
-- days.
relativeDate :: Instant -> Instant -> Int
relativeDate t1 t2 = (Time.localJulianDay t2) - (Time.localJulianDay t1)

lastWatered :: Instant -> Plant -> String
lastWatered now plant = case Plant.lastWatered plant of
  Nothing -> "not yet"
  Just t -> case relativeDate now t of
    -1        -> "yesterday"
    0         -> "today"
    n | n < 0 -> show (-n) <> " days ago"
    _         -> "in the future"

lastFertilized :: Instant -> Plant -> String
lastFertilized now plant = case Plant.lastFertilized plant of
  Nothing -> "not yet"
  Just t -> case relativeDate now t of
    -1        -> "yesterday"
    0         -> "today"
    n | n < 0 -> show (-n) <> " days ago"
    _         -> "in the future"

nextWater :: Instant -> KnownPlant -> String
nextWater now plant = case relativeDate now (Care.nextWater now plant) of
  -1        -> "Needs water since yesterday"
  0         -> "Needs water today"
  1         -> "Needs water tomorrow"
  n | n < 0 -> "Needs water since " <> show (-n) <> " days"
  n         -> "Water in " <> show n <> " days"

dropletOpacity :: Instant -> KnownPlant -> Number
dropletOpacity now plant = case relativeDate now (Care.nextWater now plant) of
  n | n <= 0 -> 1.0
  n -> 0.2 + 0.8 / (sqrt $ 2.0 + Int.toNumber n)

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

renderSpanP :: String -> String -> Html Unit
renderSpanP label value = Html.p $ do
  Html.span $ Html.text label
  Html.text value

type PlantDetailElements =
  { infoBlock :: Element
  , buttonWatered :: Element
  , buttonWateredFertilized :: Element
  }

type PlantElements =
  { statusLine :: Element
  , infoBlock :: Element
  , buttonWatered :: Element
  , buttonWateredFertilized :: Element
  }

renderPlantItem :: Instant -> KnownPlant -> Html Unit
renderPlantItem now knownPlant =
  let
    Plant plant = knownPlant.plant
    Species species = knownPlant.species
    _href = "/app/plant/" <> plant.id
  in
    Html.div $ do
      Html.addClass "plant-item"

      outer    <- ask
      expanded <- liftEffect $ Var.create false

      let
        expand :: Aff Unit
        expand = do
          -- We need to add these classes with a delay in between; when they get
          -- added simultaneously, the css transition does not take effect.
          liftEffect $ Html.withElement outer $ Html.addClass "expanded"
          Aff.delay (Milliseconds 1.0)
          liftEffect $ Html.withElement outer $ Html.addClass "unveiled"

        collapse :: Aff Unit
        collapse = do
          liftEffect $ Html.withElement outer $ Html.removeClass "unveiled"
          -- This timing is coordinated with the css transition.
          Aff.delay (Milliseconds 60.0)
          liftEffect $ Html.withElement outer $ Html.removeClass "expanded"

      statusLine <- Html.div $ do
        Html.setId plant.id
        Html.addClass "plant"
        Html.img (speciesImageUrl knownPlant.species) species.name $
          Html.addClass "plant-icon"
        Html.h2 $ Html.text plant.species
        statusLine <- Html.p $ do
          Html.img "/droplet.svg" "droplet" $ do
            Html.addClass "droplet"
            Html.setOpacity $ dropletOpacity now knownPlant
          Html.text $ nextWater now knownPlant
          ask
        Html.onClick $ Var.get expanded >>= case _ of
          true -> do
            Var.set expanded false
            Aff.launchAff_ collapse
          false -> do
            Var.set expanded true
            Aff.launchAff_ expand

        pure statusLine

      detailElements <- Html.div $ do
        Html.addClass "plant-details"
        renderDetails now knownPlant

      installClickHandlers knownPlant
        { statusLine
        , infoBlock: detailElements.infoBlock
        , buttonWatered: detailElements.buttonWatered
        , buttonWateredFertilized: detailElements.buttonWateredFertilized
        }

      pure unit

renderDetails :: Instant -> KnownPlant -> Html PlantDetailElements
renderDetails now knownPlant =
  let
    Plant plant = knownPlant.plant
    Species species = knownPlant.species
  in do
    Html.p $ do
      Html.addClass "multi"
      Html.text species.waterRemark
    Html.p $ do
      Html.addClass "multi"
      Html.text species.fertilizeRemark

    renderSpanP "water" $
      " every " <> (show species.waterDaysSummer) <> " days"
    renderSpanP "fertilize" $
      " every " <> (show species.fertilizeDaysSummer) <> " days"

    infoBlock <- Html.div $ do
      renderInfoBlock now knownPlant
      ask

    buttonWatered <- Html.button $ do
      Html.text "watered"
      ask

    buttonWateredFertilized <- Html.button $ do
      Html.text "watered + fertilized"
      ask

    pure { infoBlock, buttonWatered, buttonWateredFertilized }

renderInfoBlock :: Instant -> KnownPlant -> Html Unit
renderInfoBlock now knownPlant =
  let
    Plant plant = knownPlant.plant
    Species species = knownPlant.species
  in do
    renderSpanP "watered" $
      " " <> (lastWatered now $ Plant plant)
    renderSpanP "fertilized" $
      " " <> (lastFertilized now $ Plant plant)

installClickHandlers :: KnownPlant -> PlantElements -> Html Unit
installClickHandlers knownPlant elements =
  let
    handleClick :: (Instant -> Plant -> Aff Plant) -> Effect Unit
    handleClick f = Aff.launchAff_ $ do
      now <- liftEffect Time.getCurrentInstant
      newPlant <- f now knownPlant.plant
      liftEffect $ Html.withElement elements.infoBlock $ do
        Html.clear
        renderInfoBlock now $ knownPlant { plant = newPlant }

      liftEffect $ Html.withElement elements.statusLine $ do
        Html.clear
        Html.img "/check.svg" "check" $ Html.addClass "droplet"
        Html.text "Watered today"

    watered = handleClick Plant.postWatered
    wateredFertilized = handleClick Plant.postWateredFertilized
  in do
    local (const elements.buttonWatered) $ Html.onClick watered
    local (const elements.buttonWateredFertilized) $ Html.onClick wateredFertilized
