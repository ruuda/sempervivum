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

import Data.Text (Text)
import Prelude hiding (id, div, head, span)
import Control.Monad (mapM_)
import Text.Blaze ((!), toValue)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 (Html, body, div, docTypeHtml, h1, h2, head, link, meta, p, title, toHtml)
import Text.Blaze.Html5.Attributes (charset, content, href, id, name, rel)

import qualified Data.ByteString.Lazy as LazyByteString

import Types (Plant (..), PlantId (..), Species (..))

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

renderPlant :: Plant -> Html
renderPlant plant =
  let
    (PlantId pid) = plantId plant
    (Species species) = plantSpecies plant
  in
    div ! id ("plant" <> (toValue $ show pid)) $ do
      h2 $ toHtml species
      p $ toHtml $ "Last watered: " <> (show $ plantLastWatered plant)
      p $ toHtml $ "Last fertilized: " <> (show $ plantLastFertilized plant)

renderPlantList :: [Plant] -> Html
renderPlantList plants = do
  h1 "Plants"
  mapM_ renderPlant plants

testPage :: Html
testPage = h1 "Hello, world"
