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
connect = do
  conn <- Sqlite.open "sempervivum.sqlite3"
  Sqlite.execute conn "pragma foreign_keys = on;" ()
  pure conn

initialize :: Sqlite.Connection -> IO ()
initialize conn = do
  Sqlite.execute conn
    " create table if not exists plants                                        \
    \ ( id      integer primary key                                            \
    \ , species text not null                                                  \
    \ );                                                                       "
    ()

  Sqlite.execute conn
    " create table if not exists events                                        \
    \ ( id       integer primary key                                           \
    \ , plant_id integer not null                                              \
    \ , time     text not null -- Encoded as ISO 8601 in UTC, with Z suffix. \n\
    \ , type     text not null -- Should be 'watered' or 'fertilized'.       \n\
    \ , foreign key (plant_id) references plants(id)                           \
    \ );                                                                       \
    \ -- We could have an index on (plant_id, type, time) to accellerate the   \
    \ -- 'last event' query, but frankly, I am never going to water so many    \
    \ -- plants in my lifetime to make the difference noticeable.              "
    ()
