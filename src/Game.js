'use strict';

exports.enableAvailablePieces = function(success, fail) {
    Game.AvailablePieces.enable();
    success({})
}

exports.disableAvailablePieces = function(success, fail) {
    Game.AvailablePieces.disable();
    success({})
}

exports.enableBoard = function(success, fail) {
    Game.Board.enable();
    success({})
}

exports.disableBoard = function(success, fail) {
    Game.Board.disable();
    success({})
}
