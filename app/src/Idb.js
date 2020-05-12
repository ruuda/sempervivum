// Sempervivum -- A plant watering tracker
// Copyright 2020 Ruud van Asseldonk
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

"use strict";

exports.openImpl = function (onError, onSuccess) {
  let openRequest = window.indexedDB.open("sempervivum", 1);
  openRequest.onerror = function(event) {
    onError(openRequest.error);
  };
  openRequest.onsuccess = function(event) {
    let db = openRequest.result;
    onSuccess(db);
  };

  return function (cancelError, onCancelError, onCancelSuccess) {
    onCancelSuccess();
  };
}
