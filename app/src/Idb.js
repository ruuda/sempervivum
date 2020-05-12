// Sempervivum -- A plant watering tracker
// Copyright 2020 Ruud van Asseldonk
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

"use strict";

exports.openImpl = function(onError, onSuccess) {
  let openRequest = window.indexedDB.open("sempervivum", 1);
  openRequest.onerror = function(event) {
    onError(openRequest.error);
  };
  openRequest.onsuccess = function(event) {
    let db = openRequest.result;
    onSuccess(db);
  };
  openRequest.onupgradeneeded = function(event) {
    let db = openRequest.result;

    db.onerror = function(event) {
      onError(db.error);
    };

    // We just have a single object store called "kv" which will store
    // key-value pairs. We are just using IndexedDB for storage, not for
    // its DB functionality.
    db.createObjectStore("kv", { keyPath: "key" });
  };

  return function (cancelError, onCancelError, onCancelSuccess) {
    onCancelSuccess();
  };
}

exports.putImpl = function(unit) {
  return function(key) {
    return function(value) {
      return function(db) {
        return function(onError, onSuccess) {
          let tx = db.transaction(["kv"], "readwrite");
          // TODO: Do we need handlers for tx.onerror/tx.onabort?

          let objectStore = tx.objectStore("kv");
          let putRequest = objectStore.put({ "key": key, "value": value });

          putRequest.onerror = function(event) {
            onError(putRequest.error);
          };
          putRequest.onsuccess = function(event) {
            onSuccess(unit);
          };

          return function (cancelError, onCancelError, onCancelSuccess) {
            onCancelSuccess();
          };
        };
      };
    };
  };
};
