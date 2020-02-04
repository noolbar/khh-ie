'use strict';

let marked = require("marked")

exports._marked = function (text) {
  return function() {
    return marked(text);
  }
};

exports._outerHTML = function (text, elem) {
  return function() {
    return elem.outerHTML = text;
  }
};
