'use strict';

exports.nextButton_ = function(name, x, y, content, aff) {
  return function(success, error) {
    var button = Game.Demo.nextButton(name, x, y, content, function() {
      var a = aff(function() {});
      a();
    })
    success(button);
  }
}

exports.prevButton_ = function(name, x, y, aff) {
  return function(success, error) {
    var button = Game.Demo.prevButton(name, x, y, function() {
      aff(function() {});
    })
    success(button);
  }
}
