// Sempervivum -- A plant watering tracker
// Copyright 2020 Ruud van Asseldonk
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

"use strict";

exports.toBlob = function(value) {
  return new Blob([JSON.stringify(value)], {type: 'application/json'});
};

// CreateObjectURL is impure, so we wrap it in an Effect.
exports.getObjectUrl = function(blob) {
  return function() {
    return URL.createObjectURL(blob);
  };
};

exports.revokeObjectUrl = function(url) {
  return function() {
    URL.revokeObjectURL(url);
  };
};
