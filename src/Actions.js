'use strict';

exports.givePiece_ = function(pieceId) {
  return function(success, error) {
    Game.Actions.givePiece(pieceId, function() {
      success({});
    })
  }
}

exports.playPiece_ = function(positionId, pieceId) {
  return function(success, error) {
    Game.Actions.playPiece(positionId, pieceId, function() {
      success({});
    })
  }
}
