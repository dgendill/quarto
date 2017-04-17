module Menus (
    showMainMenu,
    hideMainMenu,
    showGame,
    hideGame
  ) where

import Prelude (Unit)
import Control.Monad.Aff (Aff)

foreign import showMainMenu :: forall e. Aff e Unit
foreign import hideMainMenu :: forall e. Aff e Unit

foreign import showGame :: forall e. Aff e Unit
foreign import hideGame :: forall e. Aff e Unit
