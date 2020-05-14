// Sempervivum -- A plant watering tracker
// Copyright 2020 Ruud van Asseldonk
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

"use strict";

exports.eqInstantImpl = function(lhs, rhs) {
  return lhs === rhs;
}

exports.ordInstantImpl = function(lt, eq, gt, lhs, rhs) {
  return lhs < rhs ? lt : lhs === rhs ? eq : gt;
}

exports.getCurrentInstant = function() {
  return new Date(Date.now());
}

exports.fromIso8601Impl = function(nothing, just, str) {
  let result = Date.parse(str);
  return result == undefined ? nothing : just(new Date(result));
}

exports.toIso8601 = function(t) {
  return t.toISOString();
}

exports.fromGregorianUtcImpl = function(year, month, day, hour, minute, second) {
  // Beware, js months start counting at 0, but dates start at 1.
  return new Date(Date.UTC(year, month - 1, day, hour, minute, second));
}

// Beware of the three pitfalls:
// 1. getDay returns the day of the week, getDate the day of the month.
// 2. getMonth is 0-based, December is month 11.
// 3. getYear returns a 2 or 3-digit number, getFullYear is the one we need.
exports.localDay = function(instant) { return instant.getDate(); }
exports.localMonth = function(instant) { return 1 + instant.getMonth(); }
exports.localYear = function(instant) { return instant.getFullYear(); }

exports.addMillisecondsImpl = function(msecs, instant) {
  return new Date(msecs + instant.getTime());
}
