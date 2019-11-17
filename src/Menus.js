'use strict';

exports.showMainMenu_ = function(onError, onSuccess) {
  console.log('show main')
  Game.showMainMenu();
  onSuccess();
}

exports.hideMainMenu_ = function(onError, onSuccess) {
  Game.hideMainMenu();
  onSuccess();
}

exports.showGame_  = function(onError, onSuccess) {
  Game.showGame();
  onSuccess();
}

exports.hideGame_ = function(onError, onSuccess) {
  Game.hideGame();
  onSuccess();
}
