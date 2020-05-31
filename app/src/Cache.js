// Sempervivum -- A plant watering tracker
// Copyright 2020 Ruud van Asseldonk
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

"use strict";

exports.openImpl = function(name) {
  return function() {
    return caches.open(name);
  }
}

exports.deleteImpl = function(name) {
  return function() {
    return caches.delete(name);
  }
}

exports.addAllImpl = function(cache, urls, unit) {
  return function() {
    return cache.addAll(urls).then(() => unit);
  }
}
