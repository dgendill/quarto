module Actions (
    givePiece,
    givePiece',
    playPiece,
    playPiece'
  ) where

import Prelude (Unit)
import Control.Monad.Aff (Aff)
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import GameGraphics (GRAPHICS)
import State (Piece, PieceID, Point, PositionID, pieceId, pointId)

foreign import givePiece_ :: forall e. Fn1 PieceID (Aff (graphics :: GRAPHICS | e) Unit)

-- | Graphicly give a piece (ids)
givePiece :: forall e. PieceID -> (Aff (graphics :: GRAPHICS | e) Unit)
givePiece = runFn1 givePiece_

-- | Graphicly give a piece (ADTs)
givePiece' :: forall e. Piece -> (Aff (graphics :: GRAPHICS | e) Unit)
givePiece' piece = givePiece (pieceId piece)

foreign import playPiece_ :: forall e. Fn2 PositionID PieceID (Aff (graphics :: GRAPHICS | e) Unit)

-- | Graphicly play a piece (ids)
playPiece :: forall e. PositionID -> PieceID -> (Aff (graphics :: GRAPHICS | e) Unit)
playPiece = runFn2 playPiece_

-- | Graphicly play a piece (ADTs)
playPiece' :: forall e. Point -> Piece -> (Aff (graphics :: GRAPHICS | e) Unit)
playPiece' point piece = playPiece (pointId point) (pieceId piece)
