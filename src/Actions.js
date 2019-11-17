'use strict';

exports.givePiece_ = function(pieceId) {
  return function(onError, onSuccess) {
    Game.Actions.givePiece(pieceId, function() {
      onSuccess();
    });
  }
}

exports.playPiece_ = function(positionId, pieceId) {
  return function(onError, onSuccess) {
    Game.Actions.playPiece(positionId, pieceId, function() {
      onSuccess();
    })
  }
}
