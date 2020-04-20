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
) where

import Data.Text (Text)
import Prelude hiding (id, div, head, span)
import Text.Blaze ((!))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 (Html, body, div, docTypeHtml, h1, head, meta, title, toHtml)
import Text.Blaze.Html5.Attributes (charset, content, id, name)

import qualified Data.ByteString.Lazy as LazyByteString

-- Wraps the given body html in html for an actual page, and encodes the
-- resulting page in utf-8.
renderPage :: Text -> Html -> LazyByteString.ByteString
renderPage pageTitle bodyHtml = renderHtml $ docTypeHtml $ do
  head $ do
    meta ! charset "utf-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    meta ! name "robots" ! content "noindex, nofollow"
    title $ toHtml pageTitle
  body $
    div ! id "content" $
      bodyHtml

testPage :: Html
testPage = h1 "Hello, world"
