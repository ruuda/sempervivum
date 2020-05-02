-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Time
  ( Instant
  , addDays
  , fromGregorianUtc
  , getCurrentInstant
  , localJulianDay
  ) where

import Prelude

import Data.Argonaut.Core (caseJsonString) as Json
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Either (Either (..))
import Data.Function.Uncurried (Fn2, Fn3, Fn5, Fn6, runFn2, runFn3, runFn5, runFn6)
import Data.Int as Int
import Data.Maybe (Maybe (..))
import Effect (Effect)

foreign import data Instant :: Type

foreign import addMillisecondsImpl :: Fn2 Number Instant Instant
foreign import eqInstantImpl :: Fn2 Instant Instant Boolean
foreign import fromGregorianUtcImpl :: Fn6 Int Int Int Int Int Int Instant
foreign import fromIso8601Impl :: Fn3 (Maybe Instant) (Instant -> Maybe Instant) String (Maybe Instant)
foreign import getCurrentInstant :: Effect Instant
foreign import ordInstantImpl :: Fn5 Ordering Ordering Ordering Instant Instant Ordering

-- Return the date of the given instant in the user's local time zone.
foreign import localDay   :: Instant -> Int
foreign import localMonth :: Instant -> Int
foreign import localYear  :: Instant -> Int

instance eqInstant :: Eq Instant where
  eq = runFn2 eqInstantImpl

instance ordInstant :: Ord Instant where
  compare = runFn5 ordInstantImpl LT EQ GT

instance decodeJsonInstant :: DecodeJson Instant where
  decodeJson = Json.caseJsonString
    (Left "Expected Instant to be a string.")
    $ fromIso8601 >>> case _ of
        Nothing -> Left "Failed to parse ISO-8601 string."
        Just t  -> Right t

fromGregorianUtc :: Int -> Int -> Int -> Int -> Int -> Int -> Instant
fromGregorianUtc = runFn6 fromGregorianUtcImpl

fromIso8601 :: String -> Maybe Instant
fromIso8601 = runFn3 fromIso8601Impl Nothing Just

-- Note that the argument is Number, not Int. The range of a signed 32-bit
-- number of milliseconds is -24.8 to +24.8 days, so an Int is unable to
-- represent long time spans.
addMilliseconds :: Number -> Instant -> Instant
addMilliseconds = runFn2 addMillisecondsImpl

addDays :: Int -> Instant -> Instant
addDays n = addMilliseconds $ (Int.toNumber n) * 24.0 * 3600.0 * 1000.0

-- Return the Julian day number of the given instant in the user's local time
-- zone. Algorithm from
-- https://en.wikipedia.org/wiki/Julian_day#Converting_Gregorian_calendar_date_to_Julian_Day_Number
xlocalJulianDay :: Instant -> Int
xlocalJulianDay t =
  let
    y = localYear t
    m = localMonth t
    d = localDay t
  in
    0
    + (1461 * (y + 4800 + (m - 14) / 12)) / 4
    + (367 * (m - 2 - 12 * ((m - 14) / 12))) / 12
    - (3 * ((y + 4900 + (m - 14) / 12) / 100)) / 4
    + d
    - 32075

-- Return the Julian day number of the given instant in the user's local time
-- zone. Algorithm from
-- https://stason.org/TULARC/society/calendars/2-15-1-Is-there-a-formula-for-calculating-the-Julian-day-nu.html
localJulianDay :: Instant -> Int
localJulianDay t =
  let
    year  = localYear t
    month = localMonth t
    day   = localDay t

    a = (14 - month) / 12
    y = year + 4800 - a
    m = month + 12 * a - 3
  in
    day + (153 * m + 2) / 5 + y * 365 + y / 4 - y / 100 + y / 400 - 32045
