-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Time
  ( Duration
  , Instant
  , add
  , fromDays
  , fromGregorianUtc
  , fromSeconds
  , getCurrentInstant
  , localJulianDay
  , subtract
  , toIso8601
  , toSeconds
  ) where

import Prelude

import Data.Argonaut.Core (caseJsonString, fromString) as Json
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Either (Either (..))
import Data.Function.Uncurried (Fn2, Fn3, Fn5, Fn6, runFn2, runFn3, runFn5, runFn6)
import Data.Int as Int
import Data.Maybe (Maybe (..))
import Effect (Effect)

foreign import data Instant :: Type

foreign import addSecondsImpl :: Fn2 Number Instant Instant
foreign import diffSecondsImpl :: Fn2 Instant Instant Number
foreign import eqInstantImpl :: Fn2 Instant Instant Boolean
foreign import fromGregorianUtcImpl :: Fn6 Int Int Int Int Int Int Instant
foreign import fromIso8601Impl :: Fn3 (Maybe Instant) (Instant -> Maybe Instant) String (Maybe Instant)
foreign import getCurrentInstant :: Effect Instant
foreign import ordInstantImpl :: Fn5 Ordering Ordering Ordering Instant Instant Ordering

-- Return the date of the given instant in the user's local time zone.
foreign import localDay   :: Instant -> Int
foreign import localMonth :: Instant -> Int
foreign import localYear  :: Instant -> Int
foreign import toIso8601  :: Instant -> String

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

instance encodeJsonInstant :: EncodeJson Instant where
  encodeJson = Json.fromString <<< toIso8601

-- Duration represents a number of seconds, but the inner value should not be
-- exposed outside of this module. Durations are signed.
newtype Duration = Duration Number
derive instance eqDuration :: Eq Duration
derive instance ordDuration :: Ord Duration

fromGregorianUtc :: Int -> Int -> Int -> Int -> Int -> Int -> Instant
fromGregorianUtc = runFn6 fromGregorianUtcImpl

fromIso8601 :: String -> Maybe Instant
fromIso8601 = runFn3 fromIso8601Impl Nothing Just

-- Note that the argument is Number, not Int. The range of a signed 32-bit
-- number of milliseconds is -24.8 to +24.8 days, so an Int is unable to
-- represent long time spans.
addSeconds :: Number -> Instant -> Instant
addSeconds = runFn2 addSecondsImpl

diffSeconds :: Instant -> Instant -> Number
diffSeconds = runFn2 diffSecondsImpl

add :: Duration -> Instant -> Instant
add (Duration secs) = addSeconds secs

subtract :: Instant -> Instant -> Duration
subtract t0 t1 = fromSeconds $ diffSeconds t0 t1

fromSeconds :: Number -> Duration
fromSeconds = Duration

fromDays :: Int -> Duration
fromDays n = fromSeconds $ 24.0 * 3600.0 * Int.toNumber n

toSeconds :: Duration -> Number
toSeconds (Duration secs) = secs

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
