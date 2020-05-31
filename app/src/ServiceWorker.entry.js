// Sempervivum -- A plant watering tracker
// Copyright 2020 Ruud van Asseldonk
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

self.addEventListener('install', function(evt) {
  evt.waitUntil(PS.ServiceWorker.onInstallPromise())
});

self.addEventListener('activate', function(evt) {
  evt.waitUntil(PS.ServiceWorker.onActivatePromise())
});

self.addEventListener('fetch', function(evt) {
  evt.respondWith(PS.ServiceWorker.onFetchPromise(evt.request)());
});
