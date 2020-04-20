-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Logger (LoggingT, runStdoutLoggingT, logInfoN)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import Control.Monad.Trans.Class (lift)

import qualified Data.Text.Lazy as LazyText
import qualified Web.Scotty.Trans as Scotty

import qualified Database
import qualified WebInterface

server :: Scotty.ScottyT LazyText.Text (LoggingT IO) ()
server = do
  Scotty.get "/" $ do
    lift $ logInfoN "Serving /"
    Scotty.setHeader "Content-Type" "text/html; charset=utf-8"
    let title = "Sempervivum"
    Scotty.raw $ WebInterface.renderPage title $ WebInterface.testPage

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

  Scotty.scottyT 8000 runStdoutLoggingT server
