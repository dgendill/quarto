import * as Board from './components/Board'
import * as AvailablePieces from './components/AvailablePieces'
import { animatePieceToDeck, animatePieceToPosition } from './Animation'
import * as Util from './Util'

export function givePiece(id, callback) {

  var piece = project.getItem({
    name : id
  });
  piece.pivot = piece.bounds.centerTop;

  var board = project.getItem({
    name : "board"
  })
  board.pivot = board.bounds.bottomCenter;

  var onDeck = project.getItem({
    name : "onDeck"
  })

  piece.gameRemoveAllListeners();
  piece.downScale();
  piece.remove();
  onDeck.addChild(piece);

  animatePieceToDeck(id, function() {
    callback();
  });

}

// PositionId -> PieceId -> Eff _ -> Eff (board :: BOARD) Unit
export function playPiece(positionId, pieceId, callback) {

  let x = parseInt(positionId.split(',')[0]);
  let y = parseInt(positionId.split(',')[1]);

  var place = project.getItem({ name : positionId });

  var piece = project.getItem({ name : pieceId });
  piece.pivot = piece.bounds.bottomCenter;
  piece.gameRemoveAllListeners();

  var playedPieces = Util.getPointLayer(x, y);
  var board = project.getItem({ name : "board" })

  piece.remove();
  place.played = pieceId;
  var p = board.matrix.transform(place.bounds.center)

  playedPieces.addChild(piece);

  animatePieceToPosition(pieceId, positionId, function() {
    callback();
  })

  Board.disable();

}
