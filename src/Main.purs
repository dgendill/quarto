module Main where

import Prelude
import State
import Data.Tuple
import Data.Maybe
import Data.Either
import Halogen.Aff as HA
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Array (length)
import GameBoard (gameBoard)
import GameSpace (gameSpace)
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafePartial)

type GameEffects eff = HA.HalogenEffects (console :: CONSOLE | eff)

renderGame :: forall e. Eff (GameEffects e) Unit
renderGame = HA.runHalogenAff do
  body <- HA.awaitBody
  el <- HA.selectElement "#board"
  case el of
    Just e -> runUI (gameBoard Nothing) unit body
    Nothing -> runUI (gameBoard Nothing) unit body

-- Nothing -> runUI (gameSpace (Tuple 1 1) Nothing) unit body
-- Just $ (unsafePartial (fromRight $ (piece Tall White Circle Hollow))
-- renderBoard :: Eff (E.HalogenEffects ()) Unit
-- renderBoard = U.runHalogenAff do
--   body <- U.awaitBody
--   el <- U.selectElement "#board"
--   runUI Container.component unit el

main = do
  renderGame
