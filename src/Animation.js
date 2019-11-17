'use strict';

exports.animatePieceToPosition_ = function(pieceId, positionId) {
  return function(error, success) {
    Game.Animation.animatePieceToPosition(pieceId, positionId, function() {
      success();
    });
  }
}

exports.animatePieceToAbsPosition_ = function(pieceId, x, y) {
  return function(error, success) {
    Game.Animation.animatePieceToAbsPositionXY(pieceId, x, y, function() {
      success();
    });
  }
}

exports.animatePieceToDeck_ = function(pieceId) {
  return function(error, success) {
    Game.Animation.animatePieceToDeck(pieceId, function() {
      success();
    });
  }
}

exports.animatePieceToHome_ = function(pieceId) {
  return function(error, success) {
    Game.Animation.animatePieceToHome(pieceId, function() {
      success();
    });
  }
}
