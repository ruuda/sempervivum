-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.HashMap.Strict as HashMap
import qualified System.Environment as Environment
import qualified System.Exit as System

import qualified Species

-- Export the species catalog as json to stdout.
exportSpecies :: IO ()
exportSpecies = do
  -- Load the species definitions in the toml files in the "species" directory.
  catalog <- Species.readCatalogOrExit "species"
  ByteString.putStr $ Aeson.encode $ HashMap.elems catalog

printUsage :: IO ()
printUsage = do
  putStrLn "Sempervivum -- A plant watering tracker\n"
  putStrLn "Usage:\n"
  putStrLn "  sempervivum export-species    Export species json to stdout"
  System.exitFailure

main :: IO ()
main = do
  args <- Environment.getArgs
  case args of
    "export-species" : [] -> exportSpecies
    _                     -> printUsage
