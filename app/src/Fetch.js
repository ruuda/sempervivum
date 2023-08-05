// Sempervivum -- A plant watering tracker
// Copyright 2020 Ruud van Asseldonk
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

"use strict";

export const fetchImpl = function(request) {
  return function() {
    return fetch(request);
  }
}

export const url = function(request) {
  return request.url;
}

export const urlPath = function(url) {
  return new URL(url).pathname;
}

export const method = function(request) {
  return request.method;
}

export const statusCode = function(response) {
  return response.status;
}

export const clone = function(response) {
  return response.clone();
}

export const readJsonImpl = function(requestOrResponse) {
  return requestOrResponse.json();
}
