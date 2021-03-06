-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
-- Copyright 2019 Ruud van Asseldonk (in Mindec, github.com/ruuda/mindec)
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Html
  ( Html
  , a
  , addClass
  , button
  , clear
  , div
  , em
  , h1
  , h2
  , img
  , imgWithFallback
  , inputFile
  , inputText
  , li
  , node
  , onClick
  , onInput
  , onTouchMove
  , onTouchStart
  , onRightClick
  , p
  , remove
  , removeClass
  , setDisabled
  , setDownload
  , setId
  , setOpacity
  , setTitle
  , span
  , text
  , ul
  , withElement
  ) where

import Control.Monad.Reader.Trans (ReaderT (..))
import Dom (Element, Position)
import Dom as Dom
import Effect (Effect)
import Prelude

-- An effect that builds nodes and appends them to the parent.
type Html a = ReaderT Element Effect a

withElement :: forall a. Element -> Html a -> Effect a
withElement container (ReaderT f) = f container

clear :: Html Unit
clear = ReaderT $ \container -> Dom.clearElement container

remove :: Html Unit
remove = ReaderT $ \container -> Dom.removeElement container

node :: forall a. String -> Html a -> Html a
node tagName (ReaderT children) =
  ReaderT $ \container -> do
    self <- Dom.createElement tagName
    result <- children self
    Dom.appendChild self container
    pure result

text :: String -> Html Unit
text value = ReaderT $ \container -> Dom.appendText value container

setId :: String -> Html Unit
setId id = ReaderT $ \container -> Dom.setId id container

setOpacity :: Number -> Html Unit
setOpacity opacity = ReaderT $ \container -> Dom.setOpacity opacity container

setTitle :: String -> Html Unit
setTitle title = ReaderT $ \self -> Dom.setAttribute "title" title self

setDisabled :: Boolean -> Html Unit
setDisabled isDisabled = ReaderT $ \self -> Dom.setDisabled isDisabled self

setDownload :: String -> Html Unit
setDownload fname = ReaderT $ \self -> Dom.setDownload fname self

addClass :: String -> Html Unit
addClass className = ReaderT $ \container -> Dom.addClass className container

removeClass :: String -> Html Unit
removeClass className = ReaderT $ \container -> Dom.removeClass className container

onClick :: Effect Unit -> Html Unit
onClick callback = ReaderT $ \container ->
  Dom.addEventListener "click" callback container

onRightClick :: Effect Unit -> Html Unit
onRightClick callback = ReaderT $ \container ->
  Dom.addRightClickListener callback container

onInput :: (String -> Effect Unit) -> Html Unit
onInput callback = ReaderT $ \container ->
  let
    getValueAndCall = do
      value <- Dom.getValue container
      callback value
  in
    Dom.addEventListener "input" getValueAndCall container

onTouchStart :: (Array Position -> Effect Unit) -> Html Unit
onTouchStart callback = ReaderT $ \container ->
  Dom.addTouchEventListener "touchstart" callback container

onTouchMove :: (Array Position -> Effect Unit) -> Html Unit
onTouchMove callback = ReaderT $ \container ->
  Dom.addTouchEventListener "touchmove" callback container

div :: forall a. Html a -> Html a
div children = node "div" children

h1 :: forall a. Html a -> Html a
h1 children = node "h1" children

h2 :: forall a. Html a -> Html a
h2 children = node "h2" children

p :: forall a. Html a -> Html a
p children = node "p" children

span :: forall a. Html a -> Html a
span children = node "span" children

em :: forall a. Html a -> Html a
em children = node "em" children

ul :: forall a. Html a -> Html a
ul children = node "ul" children

li :: forall a. Html a -> Html a
li children = node "li" children

button :: forall a. Html a -> Html a
button children = node "button" children

a :: forall a. String -> Html a -> Html a
a href (ReaderT children) = ReaderT $ \container -> do
  self <- Dom.createElement "a"
  Dom.setAttribute "href" href self
  result <- children self
  Dom.appendChild self container
  pure result

img :: forall a. String -> String -> Html a -> Html a
img src alt (ReaderT children) = ReaderT $ \container -> do
  self <- Dom.createElement "img"
  Dom.setAttribute "src" src self
  Dom.setAttribute "alt" alt self
  result <- children self
  Dom.appendChild self container
  pure result

imgWithFallback :: forall a. String -> String -> String -> Html a -> Html a
imgWithFallback src fallbackSrc alt (ReaderT children) = ReaderT $ \container -> do
  self <- Dom.createElement "img"
  let
    onError = do
      Dom.unsetOnError self
      Dom.setAttribute "src" fallbackSrc self
  Dom.setOnError onError self
  Dom.setAttribute "src" src self
  Dom.setAttribute "alt" alt self
  result <- children self
  Dom.appendChild self container
  pure result

inputText :: forall a. String -> Html a -> Html a
inputText placeholder (ReaderT children) =
  node "input" $ ReaderT $ \self -> do
    Dom.setAttribute "placeholder" placeholder self
    children self

inputFile :: forall a. String -> Html a -> Html a
inputFile accept (ReaderT children) =
  node "input" $ ReaderT $ \self -> do
    Dom.setAttribute "type" "file" self
    Dom.setAttribute "accept" accept self
    children self
