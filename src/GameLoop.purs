module GameLoop (
    mainMenuHandler
  ) where

import Prelude

import AI (bestGive, bestPlay, getPoint, getPoint')
import Actions (givePiece, playPiece')
import Control.Coroutine (Process, pullFrom)
import Control.Coroutine as CR
import Control.Monad.Trans.Class (lift)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(Nothing, Just), isNothing)
import Data.Newtype (unwrap)
import DataTypes (GameState, GameType(TwoPlayerSameTerminal))
import Demo (runDemo)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (Ref, modify_, new, read)
import Game (disableAvailablePieces, enableAvailablePieces, enableBoard)
import GameGraphics (PaperItem, hideGivePieceText, hideMessage, hidePlayPieceText, itemName, showGivePieceText, showMessage, showPlayPieceText)
import GameLogic (Driver, basicWinHandler, boardEventProducer, initialState, pieceProducer, setOnDeck, startGameState, twoPlayerSameTerminalGive, twoPlayerSameTerminalPlay)
import Menus (hideMainMenu, showGame)
import State (pieceId, unsafePieceIdToPiece)
import UI (onClick, onEvent)
import Web.Event.Event (type_)
import Web.Event.Internal.Types (Event)

handlePieceEvents :: Ref GameState -> (PaperItem -> Event -> Aff Unit)
handlePieceEvents gs = \item evt -> do
  let pieceName = itemName item
  let eventType = unwrap (type_ evt)
  log $ pieceName
  log $ eventType


setGameType :: Ref GameState -> GameType -> Aff Unit
setGameType state t = liftEffect $ modify_ (_ { gametype = t }) state 


mainMenuHandler :: String -> Aff Unit
mainMenuHandler e = do

  case e of
    "new-game" -> do
      state <- liftEffect $ new initialState
      hideMainMenu
      showGame
      setGameType state TwoPlayerSameTerminal
      startGameState state
      showGivePieceText

      let driver = basicWinHandler state
      let startGame = (CR.runProcess (twoPlayerSameTerminalGameLoop driver))

      void $ attempt startGame
      pure unit


    -- "new-game-remote" -> do
    --   hideMainMenu
    --   state <- liftEffect $ new initialTwoPlayerState
    --   let driver = basicWinHandler io state -- (\s w -> pure unit)
    --   setupRemoteMenu driver
    -- "new-game-single-player" -> do
    --   state <- liftEffect $ new initialState
    --   hideMainMenu
    --   showGame
    --   setGameType state SinglePlayer
    --   startGameState state
    --   showGivePieceText
    --   let driver = basicWinHandler io state -- (\s w -> pure unit)
    --   let startGame = (CR.runProcess (singlePlayerGameLoop driver))
    --   io.subscribe (consumer (\message -> do
    --     case message of
    --       (WinMenu.PlayAgain) -> do
    --         resetBoard state
    --         startGameState state
    --         void $ attempt startGame
    --         pure $ Nothing
    --       (WinMenu.MainMenu) -> do
    --         resetBoard state
    --         pure $ Just unit
    --       (WinMenu.Winner set) -> do
    --         pure $ Nothing
    --   ))
    --   void $ attempt startGame
    --   pure unit
    "how-to-play" -> do
      hideMainMenu
      showGame
      hideGivePieceText
      runDemo
      pure unit

    _ -> log $ "Bad input: " <> e <> " is not handled in main menu handler."


twoPlayerSameTerminalGameLoop :: forall a
   . Driver a
  -> Process Aff Unit
twoPlayerSameTerminalGameLoop driver = do
  let state = driver.state
  pullFrom (onClick state (\_ event -> do
    twoPlayerSameTerminalGive state event
    enableBoard
  )) pieceProducer
  lift $ hideGivePieceText
  lift $ showPlayPieceText
  pullFrom (onEvent "mouseup" state \_ event -> do
    twoPlayerSameTerminalPlay state event
    enableAvailablePieces
    hidePlayPieceText
    showGivePieceText
    pure unit
  ) boardEventProducer

  ww <- lift $ driver.winHandler state (\_ ->
    showMessage "You win!"
  )

  if (isNothing ww)
    then twoPlayerSameTerminalGameLoop driver
    else pure unit


singlePlayerGameLoop :: forall a
   . Driver a
  -> Process Aff Unit
singlePlayerGameLoop driver = do
  let state = driver.state
  pullFrom (onClick state (\_ event -> do
    hideGivePieceText
    showMessage "Wait for the computer to play..."
    setOnDeck state event.piece
    disableAvailablePieces
    givePiece event.piece
  )) pieceProducer

  s <- lift $ liftEffect $ read state

  traverse_ (\ondeck' -> do
    let ondeck = unsafePieceIdToPiece ondeck'
    play <- lift $ liftEffect $ bestPlay s.board ondeck
    case play of
      Just p -> do
        lift $ playPiece' (getPoint p) ondeck

        lift $ twoPlayerSameTerminalPlay state { position : getPoint' p }

        w <- lift $ driver.winHandler state (\_ ->
          showMessage "You lose!"
        )

        lift $ when (isNothing w) $ do pure unit

        lift $ enableAvailablePieces

        ss <- lift $ liftEffect $ read state
        mgive <- lift $ liftEffect $ bestGive ss.board

        lift $ traverse_ (\piece -> do
          setOnDeck state (pieceId piece)
          disableAvailablePieces
          givePiece (pieceId piece)
          enableBoard
        ) mgive

        lift $ hideMessage
        lift $ showPlayPieceText

        pullFrom (onEvent "mouseup" state \_ event -> do
          twoPlayerSameTerminalPlay state event
          enableAvailablePieces
          hidePlayPieceText
          showGivePieceText
          pure unit
        ) boardEventProducer

        ww <- lift $ driver.winHandler state (\_ -> do
          showMessage "You win!"
        )

        lift $ when (isNothing ww) $ pure unit

        singlePlayerGameLoop driver

      Nothing -> do
        lift $ showMessage "The legendary computer walks away."
        lift $ log "Computer couldn't decide."
  ) s.ondeck
