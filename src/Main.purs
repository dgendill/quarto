module Main where

import Prelude
import Control.Monad.Aff (Canceler, runAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF, newRef)
import DOM (DOM)
import GameGraphics (GRAPHICS, createMainMenu, drawAvailablePieces, drawAvailablePiecesLayout, drawBoard, init, loadAssets, pieceAssets)
import GameLogic (initialTwoPlayerState)
import GameLoop (mainMenuHandler)
import Menus (hideGame, showMainMenu)
import State (makePieces, pieceId)
import WebRTC.RTC (RTC)


main :: forall t55.
      Eff
        ( console :: CONSOLE
        , avar :: AVAR
        , dom :: DOM
        , err :: EXCEPTION
        , exception :: EXCEPTION
        , graphics :: GRAPHICS
        , random :: RANDOM
        , ref :: REF
        , rtc :: RTC
        | t55
        )
        (Canceler
           ( console :: CONSOLE
           , avar :: AVAR
           , dom :: DOM
           , err :: EXCEPTION
           , exception :: EXCEPTION
           , graphics :: GRAPHICS
           , random :: RANDOM
           , ref :: REF
           , rtc :: RTC
           | t55
           )
        )
main = runAff logShow (const $ pure unit) $ do
  assets <- loadAssets pieceAssets (map pieceId makePieces)
  init
  drawBoard
  drawAvailablePiecesLayout
  drawAvailablePieces
  showMainMenu
  hideGame
  state <- liftEff $ newRef initialTwoPlayerState
  createMainMenu mainMenuHandler
