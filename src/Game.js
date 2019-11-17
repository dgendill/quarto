'use strict';

const _id = function() {};
const _cancelSuccess = function (cancelError, cancelerError, cancelerSuccess) {
  cancelerSuccess();
};

exports.enableAvailablePieces_ = function(fail, success) {
    Game.AvailablePieces.enable();
    success();
    return _cancelSuccess;
}

exports.disableAvailablePieces_ = function(fail, success) {
    Game.AvailablePieces.disable();
    success();
    return _cancelSuccess;
}

exports.enableBoard_ = function(fail, success) {
    Game.Board.enable();
    success();
    return _cancelSuccess;
}

exports.disableBoard_ = function(fail, success) {
    Game.Board.disable();
    success();
    return _cancelSuccess;
}
