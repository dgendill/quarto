module Menus (
    showMainMenu,
    hideMainMenu,
    showGame,
    hideGame
  ) where

import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff(..), fromEffectFnAff)
import Prelude (Unit)

foreign import showMainMenu_ :: EffectFnAff Unit
showMainMenu = fromEffectFnAff showMainMenu_

foreign import hideMainMenu_ :: EffectFnAff Unit
hideMainMenu = fromEffectFnAff hideMainMenu_

foreign import showGame_ :: EffectFnAff Unit
showGame = fromEffectFnAff showGame_

foreign import hideGame_ :: EffectFnAff Unit
hideGame = fromEffectFnAff hideGame_
