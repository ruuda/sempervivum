-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Logger (LoggingT, runStdoutLoggingT, logDebugN)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Time.Clock as Clock
import qualified Database.SQLite.Simple as Sqlite
import qualified System.Directory as Directory
import qualified Web.Scotty.Trans as Scotty

import Species (Catalog)

import qualified Database
import qualified Species

server
  :: Catalog
  -> Sqlite.Connection
  -> Scotty.ScottyT LazyText.Text (LoggingT IO) ()
server catalog conn = do
  Scotty.get "/style.css"  $ do
    Scotty.setHeader "content-type" "text/css"
    Scotty.file "app/style.css"

  Scotty.get "/manifest.json"  $ do
    Scotty.setHeader "content-type" "application/manifest+json"
    Scotty.file "app/manifest.json"

  Scotty.get (Scotty.regex "^/(.*)\\.svg$") $ do
    slug <- Scotty.param "1"
    Scotty.setHeader "content-type" "image/svg+xml"
    Scotty.file $ "assets/" <> slug <> ".svg"

  Scotty.get (Scotty.regex "^/(.*)\\.webp$")  $ do
    slug <- Scotty.param "1"
    lift $ logDebugN $ "Serving image " <> (Text.pack slug)
    let photoFname = "photos/" <> slug <> ".webp"
    hasPhoto <- liftIO $ Directory.doesFileExist photoFname
    case hasPhoto of
      True -> do
        Scotty.setHeader "content-type" "image/webp"
        Scotty.file photoFname
      False -> do
        -- Fall back to a generic icon if we don't have a photo.
        Scotty.setHeader "content-type" "image/svg+xml"
        Scotty.file "assets/plant.svg"

  Scotty.get "/" $ do
    Scotty.setHeader "content-type" "text/html; charset=utf-8"
    Scotty.file "app/index.html"

  Scotty.get (Scotty.regex "^/(.*)\\.js$") $ do
    slug <- Scotty.param "1"
    Scotty.setHeader "content-type" "text/javascript"
    Scotty.file $ "app/output/" <> slug <> ".js"

  Scotty.get "/species.json"  $ do
    Scotty.json $ HashMap.elems catalog

  Scotty.get "/plants.json"  $ do
    plants <- liftIO $ Database.listPlants conn
    Scotty.json plants

  Scotty.post "/plants/:id/watered" $ do
    plantId <- Scotty.param "id"
    now <- liftIO $ Clock.getCurrentTime
    liftIO $ Database.recordWatered conn plantId now
    Scotty.redirect $ "/plants#plant" <> (LazyText.pack $ show plantId)

  Scotty.post "/plants/:id/fertilized" $ do
    plantId <- Scotty.param "id"
    now <- liftIO $ Clock.getCurrentTime
    liftIO $ Database.recordFertilized conn plantId now
    Scotty.redirect $ "/plants#plant" <> (LazyText.pack $ show plantId)

  Scotty.post "/plants/:id/watered-fertilized" $ do
    plantId <- Scotty.param "id"
    now <- liftIO $ Clock.getCurrentTime
    liftIO $ Database.recordWatered conn plantId now
    liftIO $ Database.recordFertilized conn plantId now
    Scotty.redirect $ "/plants#plant" <> (LazyText.pack $ show plantId)

main :: IO ()
main = do
  -- When the runtime detects that stdout is not connected to a console, it
  -- defaults to block buffering instead of line buffering. When running under
  -- systemd, this prevents log messages (which are written to stdout) from
  -- showing up until the buffer is flushed. Therefore, explicitly select line
  -- buffering, to enforce a flush after every newline.
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  conn <- Database.connect
  Database.initialize conn

  -- Load the species definitions in the toml files in the "species" directory.
  catalog <- Species.readCatalogOrExit "species"

  Scotty.scottyT 8000 runStdoutLoggingT $ server catalog conn
