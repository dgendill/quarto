module Animation (
    animatePieceToHome,
    animatePieceToPosition,
    animatePieceToDeck,
    animatePieceToAbsPosition
  ) where

import Prelude (Unit)
import Control.Monad.Aff (Aff)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import GameGraphics (GRAPHICS)
import State (PieceID, PositionID)

foreign import animatePieceToHome_ :: forall e. Fn1 PieceID (Aff (graphics :: GRAPHICS | e) Unit)
animatePieceToHome = runFn1 animatePieceToHome_

foreign import animatePieceToPosition_ :: forall e. Fn2 PieceID PositionID (Aff (graphics :: GRAPHICS | e) Unit)
animatePieceToPosition = runFn2 animatePieceToPosition_

foreign import animatePieceToDeck_ :: forall e. Fn1 PieceID (Aff (graphics :: GRAPHICS | e) Unit)
animatePieceToDeck = runFn1 animatePieceToDeck_

foreign import animatePieceToAbsPosition_ :: forall e. Fn3 PieceID Int Int (Aff (graphics :: GRAPHICS | e) Unit)
animatePieceToAbsPosition = runFn3 animatePieceToAbsPosition_
