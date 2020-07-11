// Sempervivum -- A plant watering tracker
// Copyright 2020 Ruud van Asseldonk
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

"use strict";

exports.fetchImpl = function(request) {
  return function() {
    return fetch(request);
  }
}

exports.url = function(request) {
  return request.url;
}

exports.urlPath = function(url) {
  return new URL(url).pathname;
}

exports.method = function(request) {
  return request.method;
}

exports.statusCode = function(response) {
  return response.status;
}

exports.clone = function(response) {
  return response.clone();
}

exports.readJsonImpl = function(requestOrResponse) {
  return requestOrResponse.json();
}
