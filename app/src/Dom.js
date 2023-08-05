// Sempervivum -- A plant watering tracker
// Copyright 2020 Ruud van Asseldonk
// Copyright 2019 Ruud van Asseldonk (in Mindec, github.com/ruuda/mindec)
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

"use strict";

export const createElement = function(tagName) {
  return function() {
    return document.createElement(tagName);
  }
}

export const appendChildImpl = function(child, container) {
  return function() {
    container.appendChild(child);
  }
}

export const clearElement = function(container) {
  return function() {
    while (container.hasChildNodes()) {
      container.removeChild(container.lastChild);
    }
  }
}

export const removeElement = function(element) {
  return function() {
    element.remove();
  }
}

export const clickElement = function(element) {
  return function() {
    element.click();
  }
}

export const appendTextImpl = function(text, container) {
  return function() {
    container.insertAdjacentText('beforeend', text);
  }
}

export const getElementByIdImpl = function(id, just, nothing) {
  return function() {
    var element = document.getElementById(id);
    if (element === null) {
      return nothing;
    } else {
      return just(element);
    }
  }
}

export const assumeElementById = function(id) {
  return function() {
    var element = document.getElementById(id);
    window.console.assert(element !== null, 'No element with id: ' + id);
    return element;
  }
}

export const body = document.body;

export const getLocationPathName = function() {
  return window.location.pathname;
}

export const getFile = function(element) {
  return function() {
    // Note that `files` is a `FileList`, not an array. We index here for
    // simplicity.
    return element.files[0];
  }
}

export const getValue = function(element) {
  return function() {
    return element.value;
  }
}

export const setAttributeImpl = function(attribute, value, element) {
  return function() {
    element.setAttribute(attribute, value);
  }
}

export const addClassImpl = function(className, element) {
  return function() {
    element.classList.add(className);
  }
}

export const removeClassImpl = function(className, element) {
  return function() {
    element.classList.remove(className);
  }
}

export const setDisabledImpl = function(isDisabled, element) {
  return function() {
    element.disabled = isDisabled;
  }
}

export const setDownloadImpl = function(fname, element) {
  return function() {
    element.download = fname;
  }
}

export const setIdImpl = function(id, element) {
  return function() {
    element.id = id;
  }
}

export const setOnErrorImpl = function(handler, element) {
  return function() {
    element.onerror = function(event) {
      handler();
    }
  }
}

export const unsetOnError = function(element) {
  return function() {
    element.onerror = null;
  }
}

export const setOpacityImpl = function(opacity, element) {
  return function() {
    element.style.opacity = opacity;
  }
}

export const addEventListenerImpl = function(eventName, callback, element) {
  return function() {
    element.addEventListener(eventName, function(evt) {
      callback();
      return false;
    });
  }
}

export const addTouchEventListenerImpl = function(eventName, callback, element) {
  return function() {
    element.addEventListener(eventName, function(evt) {
      callback(evt.changedTouches)();
      return false;
    });
  }
}

export const addRightClickListenerImpl = function(callback, element) {
  return function() {
    element.addEventListener('mousedown', function(evt) {
      if (evt.button == 2) {
        callback();
      }
      return false;
    });
    // Also prevent the context menu, now that we added a custom behavior for
    // right click.
    element.addEventListener('contextmenu', function(evt) {
      evt.preventDefault();
    });
  }
}

export const scrollIntoView = function(element) {
  return function() {
    return element.scrollIntoView();
  }
}
