import * as Util from './Util'
import * as AvailablePieces from './components/AvailablePieces'
import * as Board from './components/Board'

export const frameHandlers = [];

export function animatePieceToDeck(id, success) {
  var piece = project.getItem({
    name : id
  });
  piece.bringToFront();

  var board = project.getItem({
    name : "board"
  })

  var p = board.bounds.topRight;

  animatePieceToAbsPosition(
    id,
    Util.addVectors(
      Util.addVectors(p, Util.toPoint(Board.rpoints.pieceOffset)),
      new Point(-50, -20)
    ),
    null,
    success
  )
}


export function animatePieceToHome(pieceId, success) {
  var piece = project.getItem({name : pieceId});
  var position = AvailablePieces.getDefaultPositions([piece])[0];
  animatePieceToAbsPosition(pieceId, position, 'center', success);
}


export function animatePieceToAbsPositionXY(pieceId, x, y, success) {
  return animatePieceToAbsPosition(pieceId, new Point(x, y), null, success);
}


// String -> Point -> Eff
export function animatePieceToAbsPosition(pieceId, targetPosition, pivot, success) {
  var piece = project.getItem({ name : pieceId});
  if (pivot) { piece.pivot = piece.bounds[pivot]; }

  function sigmoid(center, curve, x) {
      return 1/(1+Math.pow(Math.E, (-curve * (x - center))));
  }

  function animate(event) {
    var piecePosition = piece.position; //bounds[pivot];
    var distance = Util.subtractVectors(targetPosition, piecePosition);

    var s = sigmoid(0, .5, distance.length);

    if (distance.length < 20) {
      var v = Util.reduceVector(distance, 1.1)
    } else {
      var reduction = 3 + ((s / 50) * 20)
      var v = Util.reduceVector(distance, reduction);
      if (v.length <= 1) v = v.normalize(1);
    }

    piece.position = Util.addVectors(piecePosition, v);

    if (distance.length <= 1) {
      Util.removeItem(frameHandlers, animate)
      success();
    }
  }

  frameHandlers.push(animate);
}


// String -> String -> Eff
export function animatePieceToPosition(pieceId, positionId, success) {
    var board = Board.parts.board;
    var space = project.getItem({ name : positionId});
    var spacePosition = Util.addVectors(
      board.matrix.transform(space.bounds.center),
      new Point(0, 10)
    )
  animatePieceToAbsPosition(pieceId, spacePosition, 'bottomCenter', success);
}
