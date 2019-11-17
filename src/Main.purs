module Main where

import Prelude

import Data.Maybe (Maybe(..))
import DataTypes (BGameState, BoardEvent, GameState, GameType(..), PieceEvent, Protocol(PState), TwoPlayerGameState, TwoPlayerGameStateExt)
import Debug.Trace (traceM)
import Debug.Trace as Debug
import Effect (Effect)
import Effect.Aff (launchAff, runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Effect.Ref as Ref
import GameGraphics (createMainMenu, drawAvailablePieces, drawAvailablePiecesLayout, drawBoard, init, loadAssets, pieceAssets)
import GameLoop (mainMenuHandler)
import Menus (hideGame, showMainMenu)
import Paper as P
import State (emptyBoard, makePieces, pieceId)

-- import Paper as P
-- import Paper.Types as Types
-- import Paper.Shape as Shape
-- import Paper.Typography as T
-- import Paper.Styling as Styling

-- main :: Effect Unit
-- main = runAff_ logShow $ do
--     liftEffect $ do
--       P.setup "board-base"
--       let s = Types.size 10.0 10.0
--       let p = Types.point 0.0 0.0
--       let rect = Shape.rectangleShape p s
--       let circ = Shape.circle (Types.point 20.0 20.0) 20.0
--       let text = T.pointtext (Types.point 100.0 100.0) {
--         content : "Hi!",
--         fillColor : "#00ff00",
--         fontSize : "2em"
--       }
    
--       Styling.style rect { fillColor : "#ff0000" }
--       Styling.style circ { justification : "left" }

--       Debug.traceM rect

--     liftEffect $ log "Running"
--     pure unit

initialTwoPlayerState :: TwoPlayerGameState
initialTwoPlayerState = {
  ondeck : Nothing,
  board : emptyBoard,
  gameinprogress : false,
  gametype : TwoPlayerRemote,
  connection : Nothing,
  channel : Nothing
}

main :: Effect Unit
main = runAff_ logShow $ do
  init
  assets <- loadAssets pieceAssets (map pieceId makePieces)
  drawBoard
  -- P.setup "board-base"
  drawAvailablePiecesLayout
  drawAvailablePieces
  showMainMenu
  hideGame
  state <- liftEffect $ Ref.new initialTwoPlayerState
  createMainMenu mainMenuHandler
  pure unit
      
-- runAff logShow (const $ pure unit) $ do
  -- init
  -- assets <- loadAssets pieceAssets (map pieceId makePieces)  
  -- drawAvailablePiecesLayout
  -- drawAvailablePieces
  -- showMainMenu
  -- hideGame
  -- state <- liftEff $ newRef initialTwoPlayerState
  -- createMainMenu mainMenuHandler
