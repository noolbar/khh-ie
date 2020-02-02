'use strict';

let marked = require("marked") 

exports._marked = function (text) {
  return marked(text);
};
