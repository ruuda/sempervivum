-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module WebInterface
( renderPage
, testPage
, renderPlantList
) where

import Control.Monad (mapM_, when)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (ZonedTime)
import Prelude hiding (id, div, head, span)
import Text.Blaze ((!), toValue)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 (Html, body, div, docTypeHtml, h1, h2, head, link, meta, p, title, toHtml)
import Text.Blaze.Html5.Attributes (charset, class_, content, href, id, name, rel)

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Clock
import qualified Data.Time.LocalTime as Clock

import Care (KnownPlant (..))
import Plant (Plant)
import Species (Catalog)

import qualified Care
import qualified Plant
import qualified Species

-- Return (t - now) in number of local days. This considers only the local date,
-- and ignores the time of the day. So if `now` is 5 seconds before midnight in
-- its local timezone, and `t` is 10 seconds after `now`, then the difference
-- would be 1 day. If both were a minute earlier, the difference would be 0
-- days.
localDaysUntil :: ZonedTime -> UTCTime -> Integer
localDaysUntil now t =
  let
    zonedT   = Clock.utcToZonedTime (Clock.zonedTimeZone now) t
    localNow = Clock.zonedTimeToLocalTime now
    localT   = Clock.zonedTimeToLocalTime zonedT
    dayNow   = Calendar.toModifiedJulianDay $ Clock.localDay localNow
    dayT     = Calendar.toModifiedJulianDay $ Clock.localDay localT
  in
    dayT - dayNow

-- Wraps the given body html in html for an actual page, and encodes the
-- resulting page in utf-8.
renderPage :: Text -> Html -> LazyByteString.ByteString
renderPage pageTitle bodyHtml = renderHtml $ docTypeHtml $ do
  head $ do
    meta ! charset "utf-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    meta ! name "robots" ! content "noindex, nofollow"
    -- TODO: Embed once I am happy with the stylesheet.
    link ! rel "stylesheet" ! href "/style.css"
    title $ toHtml pageTitle
  body $
    div ! id "content" $
      bodyHtml

renderPlant :: ZonedTime -> KnownPlant -> Html
renderPlant now knownPlant =
  let
    KnownPlant plant species = knownPlant
    waterAt = Care.nextWater (Clock.zonedTimeToUTC now) knownPlant
    waterNextRelDay = now `localDaysUntil` waterAt
    waterNextText = case waterNextRelDay of
      -1        -> "Needs water since yesterday"
      0         -> "Needs water today"
      1         -> "Needs water tomorrow"
      n | n < 0 -> "In need of water for " <> (show (-n)) <> " days"
      n         -> "Needs water in " <> (show n) <> " days"
    waterPrevText = case Plant.lastWatered plant of
      Nothing -> "Never watered before"
      Just t -> case now `localDaysUntil` t of
        -1        -> "Watered yesterday"
        0         -> "Watered today"
        n | n < 0 -> "Watered " <> (show (-n)) <> "days ago"
        _         -> "Watered some time in the future"
    fertilizePrevText = case Plant.lastFertilized plant of
      Nothing -> "Never fertilized before"
      Just t -> case now `localDaysUntil` t of
        -1        -> "Fertilized yesterday"
        0         -> "Fertilized today"
        n | n < 0 -> "Fertilized " <> (show (-n)) <> "days ago"
        _         -> "Fertilized some time in the future"
  in
    div
      ! id ("plant" <> (toValue $ show $ Plant.id plant))
      ! class_ "plant"
      $ do
        h2 $ toHtml $ Plant.species plant
        p $ toHtml $ waterNextText
        p $ toHtml $ waterPrevText
        p $ toHtml $ fertilizePrevText
        p $ toHtml $ "Water every " <> (show $ Species.waterDaysSummer species) <> " days"
        p $ toHtml $ Species.waterRemark species
        p $ toHtml $ Species.fertilizeRemark species

renderPlantList :: Catalog -> ZonedTime -> [Plant] -> Html
renderPlantList catalog now plants =
  let
    (knowns, unknowns) = Care.matchPlants catalog plants
    nowUtc         = Clock.zonedTimeToUTC now
    -- Ignore events in the past 8 hours for ordering purposes, to keep the
    -- plant order stable after ticking off a task. NominalDiffTime converts
    -- to and from integers as seconds.
    discardAfter = Clock.addUTCTime (fromInteger $ -3600 * 8) nowUtc
    plantsOrd = sortOn (Care.nextWater nowUtc . Care.discardEventsAfter discardAfter) knowns
  in do
    h1 "Plants"

    case knowns of
      [] -> p "You don’t have any plants yet."
      _  -> mapM_ (renderPlant now) plantsOrd

    when (not $ null $ unknowns) $
      p $ toHtml $ "Some plants could not be displayed "
        <> "because they are missing from the species catalog: "
        <> Text.intercalate ", " (fmap Plant.species unknowns)
        <> "."

testPage :: Html
testPage = h1 "Hello, world"
