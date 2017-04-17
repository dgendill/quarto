module Main where

import Prelude
import Control.Monad.Aff (runAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Ref (newRef)
import GameGraphics (createMainMenu, drawAvailablePieces, drawAvailablePiecesLayout, drawBoard, init, loadAssets, pieceAssets)
import GameLogic (initialTwoPlayerState)
import GameLoop (mainMenuHandler)
import Menus (hideGame, showMainMenu)
import State (makePieces, pieceId)


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
