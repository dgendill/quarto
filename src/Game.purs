module Game (
    enableAvailablePieces,
    disableAvailablePieces,
    enableBoard,
    disableBoard
  ) where

import Prelude (Unit)
import Control.Monad.Aff (Aff)
import GameGraphics (GRAPHICS)

-- Graphically disable/enable pieces and the board
foreign import enableAvailablePieces :: forall e. Aff (graphics :: GRAPHICS | e) Unit
foreign import disableAvailablePieces :: forall e. Aff (graphics :: GRAPHICS | e) Unit
foreign import enableBoard :: forall e. Aff (graphics :: GRAPHICS | e) Unit
foreign import disableBoard :: forall e. Aff (graphics :: GRAPHICS | e) Unit
