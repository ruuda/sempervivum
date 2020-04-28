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
  result = Date.parse(str);
  return result == undefined ? nothing : just(result);
}

exports.fromGregorianUtcImpl = function(year, month, day, hour, minute, second) {
  // Beware, js months start counting at 0, but dates start at 1.
  return new Date(Date.UTC(year, month - 1, day, hour, minute, second));
}
