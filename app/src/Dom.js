// Sempervivum -- A plant watering tracker
// Copyright 2020 Ruud van Asseldonk
// Copyright 2019 Ruud van Asseldonk (in Mindec, github.com/ruuda/mindec)
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

"use strict";

exports.createElement = function(tagName) {
  return function() {
    return document.createElement(tagName);
  }
}

exports.appendChildImpl = function(child, container) {
  return function() {
    container.appendChild(child);
  }
}

exports.clearElement = function(container) {
  return function() {
    while (container.hasChildNodes()) {
      container.removeChild(container.lastChild);
    }
  }
}

exports.clickElement = function(element) {
  return function() {
    element.click();
  }
}

exports.appendTextImpl = function(text, container) {
  return function() {
    container.insertAdjacentText('beforeend', text);
  }
}

exports.getElementByIdImpl = function(id, just, nothing) {
  return function() {
    var element = document.getElementById(id);
    if (element === null) {
      return nothing;
    } else {
      return just(element);
    }
  }
}

exports.assumeElementById = function(id) {
  return function() {
    var element = document.getElementById(id);
    window.console.assert(element !== null, 'No element with id: ' + id);
    return element;
  }
}

exports.body = document.body;

exports.getLocationPathName = function() {
  return window.location.pathname;
}

exports.getFile = function(element) {
  return function() {
    // Note that `files` is a `FileList`, not an array. We index here for
    // simplicity.
    return element.files[0];
  }
}

exports.getValue = function(element) {
  return function() {
    return element.value;
  }
}

exports.setAttributeImpl = function(attribute, value, element) {
  return function() {
    element.setAttribute(attribute, value);
  }
}

exports.addClassImpl = function(className, element) {
  return function() {
    element.classList.add(className);
  }
}

exports.removeClassImpl = function(className, element) {
  return function() {
    element.classList.remove(className);
  }
}

exports.setDisabledImpl = function(isDisabled, element) {
  return function() {
    element.disabled = isDisabled;
  }
}

exports.setDownloadImpl = function(fname, element) {
  return function() {
    element.download = fname;
  }
}

exports.setIdImpl = function(id, element) {
  return function() {
    element.id = id;
  }
}

exports.setOnErrorImpl = function(handler, element) {
  return function() {
    element.onerror = function(event) {
      handler();
    }
  }
}

exports.unsetOnError = function(element) {
  return function() {
    element.onerror = null;
  }
}

exports.setOpacityImpl = function(opacity, element) {
  return function() {
    element.style.opacity = opacity;
  }
}

exports.addEventListenerImpl = function(eventName, callback, element) {
  return function() {
    element.addEventListener(eventName, function(evt) {
      callback();
      return false;
    });
  }
}

exports.scrollIntoView = function(element) {
  return function() {
    return element.scrollIntoView();
  }
}
