-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Database
( addPlant
, connect
, initialize
, listPlants
, recordFertilized
, recordWatered
) where

import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sqlite

import Plant (Plant (Plant), PlantId, SpeciesName)

import qualified Plant

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

addPlant :: Sqlite.Connection -> SpeciesName -> IO PlantId
addPlant conn species = do
  Sqlite.execute conn "insert into plants (species) values (?);" [species]
  Sqlite.lastInsertRowId conn

recordEvent :: Text -> Sqlite.Connection -> PlantId -> UTCTime -> IO ()
recordEvent eventType conn plantId time =
  Sqlite.execute conn
    "insert into events (plant_id, time, type) values (?, ?, ?);"
    (plantId, time, eventType)

recordWatered :: Sqlite.Connection -> PlantId -> UTCTime -> IO ()
recordWatered = recordEvent "watered"

recordFertilized :: Sqlite.Connection -> PlantId -> UTCTime -> IO ()
recordFertilized = recordEvent "fertilized"

listPlants :: Sqlite.Connection -> IO [Plant]
listPlants conn =
  let
    -- This loads the entire database into memory, and assocates all events with
    -- the correct plant, in such a way that plants without events still show
    -- up. It is terribly inefficent, but the amount of plants and events that
    -- we are ever going to have is going to be so modest that it is not a
    -- problem at all.
    addWatered    time plant = plant { Plant.watered    = time : Plant.watered plant }
    addFertilized time plant = plant { Plant.fertilized = time : Plant.fertilized plant }

    insertPlant plants (plantId, species) =
      HashMap.insert
        plantId
        (Plant
          { Plant.id = plantId
          , Plant.species = species
          , Plant.watered = []
          , Plant.fertilized = []
          }
        )
        plants

    insertEvent plants (plantId, time, "watered") =
      HashMap.adjust (addWatered time) plantId plants

    insertEvent plants (plantId, time, "fertilized") =
      HashMap.adjust (addFertilized time) plantId plants

    insertEvent _plants (_plantId, _time, wrongType) =
      error $ Text.unpack $ "Invalid event type: " <> wrongType

  in do
   -- This is not in a transaction, but that is fine because the database is
   -- append-only.
   plantRows <- Sqlite.query conn "select id, species from plants" ()
   eventRows <- Sqlite.query conn "select plant_id, time, type from events order by time asc" ()
   let plants = foldl' insertPlant HashMap.empty plantRows
   pure $ fmap snd $ HashMap.toList $ foldl' insertEvent plants eventRows
