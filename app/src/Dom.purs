-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
-- Copyright 2019 Ruud van Asseldonk (in Mindec, github.com/ruuda/mindec)
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Dom
  ( Element
  , addClass
  , addEventListener
  , appendChild
  , appendText
  , assumeElementById
  , body
  , clearElement
  , clickElement
  , createElement
  , getValue
  , getElementById
  , getLocationPathName
  , removeClass
  , setAttribute
  , setDisabled
  , setDownload
  , setId
  , setOpacity
  , scrollIntoView
  ) where

import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3)
import Effect (Effect)
import Prelude
import Data.Maybe (Maybe (..))

foreign import data Element :: Type

foreign import assumeElementById :: String -> Effect Element
foreign import body :: Element
foreign import clearElement :: Element -> Effect Unit
foreign import createElement :: String -> Effect Element
foreign import clickElement :: Element -> Effect Unit
foreign import getLocationPathName :: Effect String
foreign import getValue :: Element -> Effect String
foreign import scrollIntoView :: Element -> Effect Unit

foreign import addClassImpl :: Fn2 String Element (Effect Unit)
foreign import addEventListenerImpl :: Fn3 String (Effect Unit) Element (Effect Unit)
foreign import appendChildImpl :: Fn2 Element Element (Effect Unit)
foreign import appendTextImpl :: Fn2 String Element (Effect Unit)
foreign import getElementByIdImpl :: Fn3 String (Element -> Maybe Element) (Maybe Element) (Effect (Maybe Element))
foreign import removeClassImpl :: Fn2 String Element (Effect Unit)
foreign import setAttributeImpl :: Fn3 String String Element (Effect Unit)
foreign import setDisabledImpl :: Fn2 Boolean Element (Effect Unit)
foreign import setDownloadImpl :: Fn2 String Element (Effect Unit)
foreign import setIdImpl :: Fn2 String Element (Effect Unit)
foreign import setOpacityImpl :: Fn2 Number Element (Effect Unit)

appendChild :: Element -> Element -> Effect Unit
appendChild child container = runFn2 appendChildImpl child container

appendText :: String -> Element -> Effect Unit
appendText text container = runFn2 appendTextImpl text container

getElementById :: String -> Effect (Maybe Element)
getElementById id = runFn3 getElementByIdImpl id Just Nothing

addClass :: String -> Element -> Effect Unit
addClass className element = runFn2 addClassImpl className element

removeClass :: String -> Element -> Effect Unit
removeClass className element = runFn2 removeClassImpl className element

setId :: String -> Element -> Effect Unit
setId id element = runFn2 setIdImpl id element

setAttribute :: String -> String -> Element -> Effect Unit
setAttribute attribute value element = runFn3 setAttributeImpl attribute value element

setDisabled :: Boolean -> Element -> Effect Unit
setDisabled isDisabled element = runFn2 setDisabledImpl isDisabled element

setDownload :: String -> Element -> Effect Unit
setDownload fname element = runFn2 setDownloadImpl fname element

setOpacity :: Number -> Element -> Effect Unit
setOpacity opacity element = runFn2 setOpacityImpl opacity element

addEventListener :: String -> Effect Unit -> Element -> Effect Unit
addEventListener eventName callback element = runFn3 addEventListenerImpl eventName callback element
