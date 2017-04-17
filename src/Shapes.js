'use strict';

exports.tooltip = function(config) {
  return function (success, error) {
    var tt = Game.Shapes.tooltip(config);
    success(tt);
  }
}

exports.labelArrow = function(config) {
  return function (success, error) {
    var la = Game.Shapes.labelArrow(config);
    success(la);
  }
}

exports.removeGraphicItem = function(gi) {
  return function (success, error) {
    gi.remove();
    success({});
  }
}

exports.removeGraphicItemByName = function(name) {
  return function (success, error) {
    Game.Shapes.removeGraphicItemByName(name);
    success({});
  }
}
