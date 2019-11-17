'use strict';

exports.tooltip_ = function(config) {
  return function (error, success) {
    var tt = Game.Shapes.tooltip(config);
    success(tt);
  }
}

exports.labelArrow_ = function(config) {
  return function (error, success) {
    var la = Game.Shapes.labelArrow(config);
    success(la);
  }
}

exports.removeGraphicItem_ = function(gi) {
  return function (error, success) {
    gi.remove();
    success();
  }
}

exports.removeGraphicItemByName_ = function(name) {
  return function (error, success) {
    Game.Shapes.removeGraphicItemByName(name);
    success();
  }
}
