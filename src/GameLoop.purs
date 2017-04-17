module GameLoop (
    mainMenuHandler
  ) where

import Prelude
import Control.Coroutine as CR
import Halogen.Aff as HA
import QHalogen.WinMenu as WinMenu
import AI (bestGive, bestPlay, getPoint, getPoint')
import Actions (givePiece, playPiece')
import Control.Coroutine (Process, consumer, pullFrom)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (Ref, modifyRef, newRef, readRef)
import Control.Monad.Trans.Class (lift)
import Control.MonadPlus (guard)
import DOM.Event.Event (Event, type_)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(Nothing, Just), fromJust, isNothing)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Demo (runDemo)
import Effects (GameEffects)
import Game (disableAvailablePieces, enableAvailablePieces, enableBoard)
import GameGraphics (PaperItem, hideGivePieceText, hideMessage, hidePlayPieceText, itemName, showGivePieceText, showMessage, showPlayPieceText)
import GameLogic (Driver, basicWinHandler, boardEventProducer, initialState, initialTwoPlayerState, pieceProducer, resetBoard, setOnDeck, startGameState, twoPlayerSameTerminalGive, twoPlayerSameTerminalPlay)
import Halogen.VDom.Driver (runUI)
import Menus (hideMainMenu, showGame)
import Partial.Unsafe (unsafePartialBecause)
import DataTypes (GameState, GameType(SinglePlayer, TwoPlayerSameTerminal))
import QHalogen.WinMenu (winMenu)
import RemoteGameLoop (setupRemoteMenu)
import State (pieceId, unsafePieceIdToPiece)
import UI (onClick, onEvent)

handlePieceEvents ::  forall e. Ref GameState -> (PaperItem -> Event -> Aff (console :: CONSOLE | e) Unit)
handlePieceEvents gs = \item evt -> do
  let pieceName = itemName item
  let eventType = unwrap (type_ evt)
  log $ pieceName
  log $ eventType


setGameType :: forall e. Ref GameState -> GameType -> GameEffects e Unit
setGameType state t = liftEff $ modifyRef state (_ { gametype = t })


mainMenuHandler :: forall e. String -> GameEffects (err :: EXCEPTION | e) Unit
mainMenuHandler e = do

  io <- ((HA.selectElement "#win-screen") >>= (traverse (runUI winMenu unit))) >>= (\m -> do
          pure $ unsafePartialBecause "#win-screen is assumed to always be there" (fromJust m)
        )

  case e of
    "new-game" -> do
      state <- liftEff $ newRef initialState
      hideMainMenu
      showGame
      setGameType state TwoPlayerSameTerminal
      startGameState state
      showGivePieceText

      let driver = basicWinHandler io state -- (\s w -> pure unit)
      let startGame = (CR.runProcess (twoPlayerSameTerminalGameLoop driver))

      io.subscribe (consumer (\message -> do
        case message of
          (WinMenu.PlayAgain) -> do
            resetBoard state
            startGameState state
            attempt startGame
            pure $ Nothing
          (WinMenu.MainMenu) -> do
            resetBoard state
            pure $ Just unit
          (WinMenu.Winner set) -> do
            showMessage "We have a winner!"
            pure $ Nothing
      ))

      attempt startGame
      pure unit

    "new-game-remote" -> do
      hideMainMenu
      state <- liftEff $ newRef initialTwoPlayerState
      let driver = basicWinHandler io state -- (\s w -> pure unit)
      setupRemoteMenu driver
    "new-game-single-player" -> do
      state <- liftEff $ newRef initialState
      hideMainMenu
      showGame
      setGameType state SinglePlayer
      startGameState state
      showGivePieceText

      let driver = basicWinHandler io state -- (\s w -> pure unit)
      let startGame = (CR.runProcess (singlePlayerGameLoop driver))

      io.subscribe (consumer (\message -> do
        case message of
          (WinMenu.PlayAgain) -> do
            resetBoard state
            startGameState state
            attempt startGame
            pure $ Nothing
          (WinMenu.MainMenu) -> do
            resetBoard state
            pure $ Just unit
          (WinMenu.Winner set) -> do
            pure $ Nothing
      ))

      attempt startGame
      pure unit

    "how-to-play" -> do
      hideMainMenu
      showGame
      hideGivePieceText
      runDemo

    _ -> log "Bad input"

twoPlayerSameTerminalGameLoop :: forall e a
   . Driver e a
  -> Process (GameEffects e) Unit
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

  -- s <- lift $ liftEff $ readRef state
  ww <- lift $ driver.winHandler state (\_ ->
    showMessage "You win!"
  )

  lift $ guard (isNothing ww)

  twoPlayerSameTerminalGameLoop driver

singlePlayerGameLoop :: forall e a
   . Driver e a
  -> Process (GameEffects e) Unit
singlePlayerGameLoop driver = do
  let state = driver.state
  pullFrom (onClick state (\_ event -> do
    hideGivePieceText
    showMessage "Wait for the computer to play..."
    setOnDeck state event.piece
    disableAvailablePieces
    givePiece event.piece
  )) pieceProducer

  s <- lift $ liftEff $ readRef state

  traverse_ (\ondeck' -> do
    let ondeck = unsafePieceIdToPiece ondeck'
    play <- lift $ liftEff $ bestPlay s.board ondeck
    case play of
      Just p -> do
        lift $ playPiece' (getPoint p) ondeck

        lift $ twoPlayerSameTerminalPlay state { position : getPoint' p }

        w <- lift $ driver.winHandler state (\_ ->
          showMessage "You lose!"
        )

        lift $ guard (isNothing w)

        lift $ enableAvailablePieces

        ss <- lift $ liftEff $ readRef state
        mgive <- lift $ liftEff $ bestGive ss.board

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

        lift $ guard (isNothing ww)

        singlePlayerGameLoop driver

      Nothing -> do
        lift $ showMessage "The legendary computer walks away."
        lift $ log "Computer couldn't decide."
  ) s.ondeck
