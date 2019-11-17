module Actions (
    givePiece,
    givePiece',
    playPiece,
    playPiece'
  ) where

import Prelude
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Effect.Aff (Aff)
import Effect.Aff.Compat (fromEffectFnAff, EffectFnAff)
import State (Piece, PieceID, Point, PositionID, pieceId, pointId)

foreign import givePiece_ :: Fn1 PieceID (EffectFnAff Unit)

-- | Graphicly give a piece (ids)
givePiece :: PieceID -> (Aff Unit)
givePiece p = fromEffectFnAff $ runFn1 givePiece_ p

-- | Graphicly give a piece (ADTs)
givePiece' :: Piece -> (Aff Unit)
givePiece' piece = givePiece (pieceId piece)

foreign import playPiece_ :: Fn2 PositionID PieceID (EffectFnAff Unit)

-- | Graphicly play a piece (ids)
playPiece :: PositionID -> PieceID -> (Aff Unit)
playPiece pid piece = fromEffectFnAff $ runFn2 playPiece_ pid piece

-- | Graphicly play a piece (ADTs)
playPiece' :: Point -> Piece -> (Aff Unit)
playPiece' point piece = playPiece (pointId point) (pieceId piece)
