module Game (
    enableAvailablePieces,
    disableAvailablePieces,
    enableBoard,
    disableBoard
  ) where

import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Prelude (Unit)

foreign import enableAvailablePieces_ :: EffectFnAff Unit
enableAvailablePieces :: Aff Unit
enableAvailablePieces = fromEffectFnAff enableAvailablePieces_

foreign import disableAvailablePieces_ :: EffectFnAff Unit
disableAvailablePieces :: Aff Unit
disableAvailablePieces = fromEffectFnAff disableAvailablePieces_

foreign import enableBoard_ :: EffectFnAff Unit
enableBoard :: Aff Unit
enableBoard = fromEffectFnAff enableBoard_

foreign import disableBoard_ :: EffectFnAff Unit
disableBoard :: Aff Unit
disableBoard = fromEffectFnAff disableBoard_
