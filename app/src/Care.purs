-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Care
  ( KnownPlant
  , MatchedPlants
  , match
  ) where

import Data.Foldable (foldl)
import Data.List (List (..), (:))
import Data.Maybe (Maybe (..))
import Foreign.Object as Object

import Plant (Plant (..), Plants (..))
import Species (Catalog, Species)

type KnownPlant =
  { plant   :: Plant
  , species :: Species
  }

type MatchedPlants =
  { knowns   :: List KnownPlant
  , unknowns :: List Plant
  }

match :: Catalog -> Plants -> MatchedPlants
match catalog (Plants plantMap) =
  let
    plants = Object.values plantMap
    prepend acc (Plant p) = case Object.lookup p.species catalog of
      Just species ->
        let
          known = { plant: Plant p, species: species }
        in
          acc { knowns = known : acc.knowns }

      Nothing ->
        acc { unknowns = (Plant p) : acc.unknowns }
  in
    foldl prepend { knowns: Nil, unknowns: Nil } plants
