// Sempervivum -- A plant watering tracker
// Copyright 2020 Ruud van Asseldonk
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

"use strict";

exports.readImpl = function(file, onLoad) {
  return function(onError, onSuccess) {
    var reader = new FileReader();
    reader.onload = function(evt) {
      onSuccess(reader.result);
    };
    reader.readAsText(file);

    return function (cancelError, onCancelError, onCancelSuccess) {
      onCancelSuccess();
    };
  };
};
