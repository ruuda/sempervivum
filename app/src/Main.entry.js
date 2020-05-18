// Sempervivum -- A plant watering tracker
// Copyright 2020 Ruud van Asseldonk
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

window.onload = function() {
  let loader = document.getElementById('loader');
   document.body.removeChild(loader);
   PS.Main.main();
};

if('serviceWorker' in navigator) {
  navigator.serviceWorker.register('/sw.js');
}
