'use strict';

exports.showMainMenu = function(success, error) {
  Game.showMainMenu();
  success({});
}

exports.hideMainMenu = function(success, error) {
  Game.hideMainMenu();
  success({});
}

exports.showGame  = function(success, error) {
  Game.showGame();
  success({});
}

exports.hideGame = function(success, error) {
  Game.hideGame();
  success({});
}
