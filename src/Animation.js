'use strict';

exports.animatePieceToPosition_ = function(pieceId, positionId) {
  return function(success, error) {
    Game.Animation.animatePieceToPosition(pieceId, positionId, function() {
      success({});
    });
  }
}

exports.animatePieceToAbsPosition_ = function(pieceId, x, y) {
  return function(success, error) {
    Game.Animation.animatePieceToAbsPositionXY(pieceId, x, y, function() {
      success({});
    });
  }
}

exports.animatePieceToDeck_ = function(pieceId) {
  return function(success, error) {
    Game.Animation.animatePieceToDeck(pieceId, function() {
      success({});
    });
  }
}

exports.animatePieceToHome_ = function(pieceId) {
  return function(success, error) {
    Game.Animation.animatePieceToHome(pieceId, function() {
      success({});
    });
  }
}
