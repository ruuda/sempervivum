-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module View
  ( renderApp
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

import AppState (AppState)
import Care (KnownPlant)
import Dom (Element)
import Html (Html)
import Plant (Plant (..))
import Species (Species (..))
import Time (Instant)

import AppState as AppState
import Care as Care
import Dom as Dom
import Html as Html
import Plant as Plant
import Species as Species
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
  n -> 0.25 + 0.75 / (1.0 + Int.toNumber n)

speciesImageUrl :: Species -> String
speciesImageUrl (Species species) =
  let
    slug
      = String.replaceAll (Pattern " ") (Replacement "_")
      $ String.replaceAll (Pattern "-") (Replacement "_")
      $ String.toLower
      $ species.name
  in
    "images/" <> slug <> ".webp"

renderPlants :: AppState -> Instant -> Html Element
renderPlants appState now = do
  ps <- liftEffect $ AppState.getMatchedPlants appState
  Html.h1 $ Html.text "Plants"
  Html.div $ do
    Html.setId "plants"
    traverse_ (renderPlantItem appState now) (Care.sortByNextWater now ps.knowns)
    case ps.unknowns of
      Nil -> pure unit
      xs  -> Html.p $ Html.text $ "And " <> (show $ List.length ps.unknowns) <> " unknown plants"
    ask

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

renderPlantItem :: AppState -> Instant -> KnownPlant -> Html Element
renderPlantItem appState now knownPlant =
  let
    Plant plant = knownPlant.plant
    Species species = knownPlant.species
  in
    Html.div $ do
      Html.addClass "plant-item"

      outer    <- ask
      expanded <- liftEffect $ Var.create false

      let
        expand :: Aff Unit
        expand = do
          liftEffect $ Var.set expanded true
          -- We need to add these classes with a delay in between; when they get
          -- added simultaneously, the css transition does not take effect.
          liftEffect $ Html.withElement outer $ Html.addClass "expanded"
          Aff.delay (Milliseconds 1.0)
          liftEffect $ Html.withElement outer $ Html.addClass "unveiled"

        collapse :: Aff Unit
        collapse = do
          liftEffect $ Var.set expanded false
          liftEffect $ Html.withElement outer $ Html.removeClass "unveiled"
          -- This timing is coordinated with the css transition.
          Aff.delay (Milliseconds 60.0)
          liftEffect $ Html.withElement outer $ Html.removeClass "expanded"

      statusLine <- Html.div $ do
        Html.setId plant.id
        Html.addClass "plant"
        Html.imgWithFallback
          -- Try the picture first. If loading that fails, fall back to the
          -- generic plant icon.
          (speciesImageUrl knownPlant.species)
          "assets/plant.svg"
          species.name $ Html.addClass "plant-icon"
        Html.h2 $ Html.text plant.species
        statusLine <- Html.p $ do
          Html.addClass "status"
          Html.img "assets/droplet.svg" "droplet" $ do
            Html.addClass "droplet"
            Html.setOpacity $ dropletOpacity now knownPlant
          Html.text $ nextWater now knownPlant
          ask
        Html.onClick $ Var.get expanded >>= case _ of
          true  -> Aff.launchAff_ collapse
          false -> Aff.launchAff_ expand

        pure statusLine

      detailElements <- Html.div $ do
        Html.addClass "plant-details"
        renderDetails now knownPlant

      installClickHandlers appState knownPlant collapse
        { statusLine
        , infoBlock: detailElements.infoBlock
        , buttonWatered: detailElements.buttonWatered
        , buttonWateredFertilized: detailElements.buttonWateredFertilized
        }

      ask

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

installClickHandlers
  :: AppState
  -> KnownPlant
  -> Aff Unit
  -> PlantElements
  -> Html Unit
installClickHandlers appState knownPlant collapse elements =
  let
    handleClick :: (Instant -> Plant -> Aff Plant) -> Effect Unit
    handleClick f = Aff.launchAff_ $ do
      now <- liftEffect $ Time.getCurrentInstant
      newPlant <- f now knownPlant.plant

      -- In parallel with swapping the status line, collapse the plant item again.
      -- We add some delay to to make the timing align nicer, so you still see
      -- a bit of the button depress, and the status animation starts first.
      -- Finally, swap out the info block after collapsing, so the pop in
      -- content is not visible.
      collapsing <- Aff.forkAff $ do
        Aff.delay (Milliseconds 66.0)
        collapse
        liftEffect $ Html.withElement elements.infoBlock $ do
          Html.clear
          renderInfoBlock now $ knownPlant { plant = newPlant }

      -- We first add a class to start the transition that fades out the current
      -- status line, then wait for that to finish playing. Then we swap in
      -- the new content, and remove the class again, to trigger the reverse
      -- animation.
      liftEffect $ Html.withElement elements.statusLine $ Html.addClass "faded"

      -- Delay tuned to the css transition duration.
      Aff.delay (Milliseconds 170.0)
      liftEffect $ Html.withElement elements.statusLine $ do
        Html.clear
        Html.img "assets/check.svg" "check" $ Html.addClass "droplet"
        Html.text "Watered today"

      -- We need to wait a bit before removing the class, otherwise the new
      -- nested content (the check image) does not pick up the style from the
      -- faded class. I've determined empirically that we need to wait more
      -- than 2ms, but 15ms is sufficient.
      Aff.delay (Milliseconds 15.0)
      liftEffect $ Html.withElement elements.statusLine $ Html.removeClass "faded"

      Aff.joinFiber collapsing

    watered = handleClick $ AppState.postWatered appState
    wateredFertilized = handleClick $ AppState.postWateredFertilized appState
  in do
    local (const elements.buttonWatered) $ Html.onClick watered
    local (const elements.buttonWateredFertilized) $ Html.onClick wateredFertilized

renderSearchResult :: AppState -> Element -> Species -> Html Unit
renderSearchResult appState plantList (Species s) = Html.li $ do
  Html.span $ Html.text s.name
  Html.button $ do
    Html.text "add"
    -- We allow only one plant instance of each species, because currently there
    -- is no good way to tell multiple plants of the same species apart, you
    -- can't give them names or anything.
    plants <- liftEffect $ AppState.getPlants appState
    Html.setDisabled $ Plant.hasSpecies s.name plants
    self <- ask
    Html.onClick $ do
      plant <- Plant.newPlant s.name
      now <- Time.getCurrentInstant
      item <- Html.withElement plantList $ renderPlantItem appState now { plant: plant, species: Species s }
      Html.withElement self $ Html.setDisabled true
      Dom.scrollIntoView item
      Aff.launchAff_ $ AppState.insertPlant appState plant

renderAddPlant :: AppState -> Element -> Html Unit
renderAddPlant appState plantList = do
  header   <- Html.h1 $ Html.text "Add new plants" *> ask
  input    <- Html.inputText "Search for species" ask
  resultUl <- Html.ul $ Html.setId "search-results" *> ask

  let
    fillResults :: String -> Effect Unit
    fillResults needle = do
      Dom.scrollIntoView header
      Html.withElement resultUl $ do
        Html.clear
        case needle of
          "" -> do
            Html.removeClass "active"
          _  -> do
            Html.addClass "active"
            case Species.search needle appState.catalog of
              [] -> do
                Html.p $ do
                  Html.addClass "multi"
                  Html.text "Nothing found. Try searching by botanical name."
                Html.p $ do
                  Html.text "If your plant is missing, a pull request to "
                  let srclink = "https://github.com/ruuda/sempervivum/tree/master/species"
                  Html.a srclink $ Html.text "add a new species"
                  Html.text " would be accepted."

              matches -> traverse_ (renderSearchResult appState plantList) matches

  local (const input) $ Html.onInput fillResults

renderManage :: AppState -> Html Unit
renderManage appState = do
  Html.h1 $ Html.text "Manage data"
  Html.p $ do
    Html.text "Your data is only stored locally on your device."
    Html.addClass "multi"
  Html.p $ do
    Html.text "Use "
    Html.em $ Html.text "export"
    Html.text " to download your data for backup or sharing purposes. Use "
    Html.em $ Html.text "restore"
    Html.text " to replace your current data with an earlier export. Use "
    Html.em $ Html.text "merge"
    Html.text " to add plants from a different export to your current data. "

  Html.button $ do
    Html.text "export"
    Html.onClick $ AppState.downloadAsJson appState

  Html.button $ do
    Html.text "restore"
    Html.onClick $ AppState.importJson appState

  Html.button $ do
    Html.text "merge"
    Html.onClick $ AppState.importJson appState

renderApp :: AppState -> Instant -> Html Unit
renderApp appState now = do
  plantList <- renderPlants appState now
  renderAddPlant appState plantList
  renderManage appState
