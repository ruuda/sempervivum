// Sempervivum -- A plant watering tracker
// Copyright 2020 Ruud van Asseldonk
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

"use strict";

export const openImpl = function(name) {
  return function() {
    return caches.open(name);
  }
}

export const deleteImpl = function(name) {
  return function() {
    return caches.delete(name);
  }
}

export const addAllImpl = function(cache, urls) {
  return function() {
    return cache.addAll(urls);
  }
}

export const putImpl = function(cache, request, response) {
  return function() {
    return cache.put(request, response);
  }
}

export const matchImpl = function(cache, request, nothing, just) {
  return function() {
    return cache.match(request).then(function(response) {
      if (response === undefined) {
        return nothing;
      } else {
        return just(response);
      }
    });
  }
}
