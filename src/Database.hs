-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Database
( connect
, initialize
) where

import qualified Database.SQLite.Simple as Sqlite

connect :: IO Sqlite.Connection
connect = Sqlite.open "sempervivum.sqlite3"

initialize :: Sqlite.Connection -> IO ()
initialize conn = do
  Sqlite.execute conn
    "CREATE TABLE IF NOT EXISTS plants ( \
    \  id   INTEGER PRIMARY KEY,         \
    \  name TEXT NOT NULL                \
    \);" ()
