'use strict';

const _id = function() {};
const _cancelSuccess = function (cancelError, cancelerError, cancelerSuccess) {
  cancelerSuccess();
};

exports.nextButton_ = function(name, x, y, content, aff, affHandler) {
  return function(success, error) {
    var button = Game.Demo.nextButton(name, x, y, content, function() {
      affHandler(aff)({});
      
    })
    success(button);
    return cancelerSuccess;
  }
}

exports.prevButton_ = function(name, x, y, aff, affHandler) {
  return function(success, error) {
    var button = Game.Demo.prevButton(name, x, y, function() {
      affHandler(aff)({});
    })
    success(button);
    return cancelerSuccess;
  }
}
