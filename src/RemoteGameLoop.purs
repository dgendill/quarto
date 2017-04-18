module RemoteGameLoop where

import Prelude
import Control.Coroutine as CR
import Halogen as H
import Halogen.Aff as HA
import QHalogen.WinMenu as WinMenu
import Control.Coroutine (Process, consumer, pullFrom)
import Control.Monad.Aff (attempt, forkAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (Ref, modifyRef, readRef)
import Control.Monad.Trans.Class (lift)
import Control.MonadPlus (guard)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Either (Either, either)
import Data.Maybe (Maybe(Just, Nothing), isNothing)
import DataTypes (PlayOrder(PlaySecond, PlayFirst), Protocol(PPlay, PGive), TwoPlayerGameState, decodeProtocol, encodeGive, encodePlay)
import Effects (GameEffects)
import Game (disableAvailablePieces, enableAvailablePieces, enableBoard)
import GameGraphics (hideGivePieceText, hideMessage, hidePlayPieceText, showGivePieceText, showMessage, showPlayPieceText)
import GameLogic (RemoteDriver, boardEventProducer, pieceProducer, resetBoard, startGameState, twoPlayerSameTerminalGive, twoPlayerSameTerminalPlay)
import Halogen.VDom.Driver (runUI)
import Menus (showGame)
import QHalogen.NetworkMenu (Message(Disconnected, Connected), networkMenu)
import State (PieceID, PositionID)
import UI (onClick, onEvent)
import WebRTC.RTC (RTCDataChannel, closeConnection, onmessageChannelOnce, send)

type TwoPlayerGameEffects e = GameEffects (err :: EXCEPTION | e )

-- | Wait for data to arrive over the data channel
awaitPeerData :: forall e. RTCDataChannel -> TwoPlayerGameEffects e (Either String Protocol)
awaitPeerData channel = (onmessageChannelOnce channel) >>= decodeProtocol >>> pure

-- | Wait for the local user to give a piece
giveStep :: forall e. Ref (TwoPlayerGameState) -> RTCDataChannel -> Process (GameEffects e) Unit
giveStep stateRef channel = do
  pullFrom (onClick stateRef (\_ event -> do
    twoPlayerSameTerminalGive stateRef event
    liftEff $ send (encodeGive event.piece) channel
  )) pieceProducer


-- | Wait for the local user to play a piece
playStep :: forall e. Ref (TwoPlayerGameState) -> RTCDataChannel -> Process (TwoPlayerGameEffects e) Unit
playStep stateRef channel = do
  pullFrom (onEvent "mouseup" stateRef (\_ event -> do
    twoPlayerSameTerminalPlay stateRef event
    enableAvailablePieces
    liftEff $ send (encodePlay event.position) channel
  )) boardEventProducer


-- | Wait for the remote player to give a piece
awaitGive :: forall e. Ref (TwoPlayerGameState) -> RTCDataChannel -> Process (TwoPlayerGameEffects e) (Maybe PieceID)
awaitGive stateRef channel = do
  give <- lift $ awaitPeerData channel
  lift $ log $ "Opponent gave " <> (show give)
  either
    (const $ pure Nothing)
    (\p -> do
        case p of
          (PGive pieceId) -> do
            lift $ do
              twoPlayerSameTerminalGive stateRef { piece : pieceId }
              enableBoard
            pure $ Just pieceId
          _ -> pure Nothing
    )
    give


-- | Wait for the remote player to play a piece
awaitPlay :: forall e. Ref (TwoPlayerGameState) -> RTCDataChannel -> Process (TwoPlayerGameEffects e) (Maybe PositionID)
awaitPlay stateRef channel = do
  play <- lift $ awaitPeerData channel
  lift $ log $ "Opponent played " <> (show play)
  either
    (const $ pure Nothing)
    (\p -> do
      case p of
        (PPlay position) -> do
          lift $
            twoPlayerSameTerminalPlay stateRef { position : position }
          pure $ Just position
        _ -> pure Nothing
    )
    play


-- | The game loop for the user who gives first
p1Loop :: forall e. (RemoteDriver (err :: EXCEPTION | e )) -> RTCDataChannel -> Process (TwoPlayerGameEffects e) (Maybe PositionID)
p1Loop driver channel = do
  let stateRef = driver.state
  lift $ showGivePieceText
  giveStep stateRef channel
  lift $ hideGivePieceText
  lift $ showMessage "Wait for your opponent to play."
  void $ awaitPlay stateRef channel
  lift $ hideMessage
  lift $ showMessage "Wait for your opponent to give you a piece."
  winner1 <- lift (driver.winHandler stateRef (\_ ->
    showMessage "You lose!"
  ))
  lift $ guard $ isNothing winner1
  void $ awaitGive stateRef channel
  lift $ hideMessage
  lift $ showPlayPieceText
  playStep stateRef channel
  lift $ hidePlayPieceText
  winner2 <- lift $ driver.winHandler stateRef (\_ ->
    showMessage "You win!"
  )
  lift $ guard $ isNothing winner2
  p1Loop driver channel


-- | The game loop for the user who plays after the other player gives first
p2Loop :: forall e. (RemoteDriver (err :: EXCEPTION | e )) -> RTCDataChannel -> Process (TwoPlayerGameEffects e) (Maybe PositionID)
p2Loop driver channel = do
  let stateRef = driver.state
  lift $ disableAvailablePieces
  lift $ showMessage "Wait for your opponent to give you a piece."
  void $ awaitGive stateRef channel
  lift $ hideMessage
  lift $ showPlayPieceText
  playStep stateRef channel
  lift $ hidePlayPieceText
  winner1 <- lift $ driver.winHandler stateRef (\_ -> do
    showMessage "You win!"
  )
  lift $ guard $ isNothing winner1
  lift $ showGivePieceText
  giveStep stateRef channel
  lift $ hideGivePieceText
  lift $ showMessage "Wait for your opponent to play."
  void $ awaitPlay stateRef channel
  lift $ hideMessage
  winner2 <- lift $ driver.winHandler stateRef (\_ -> do
    showMessage "You lose!"
  )
  lift $ guard $ isNothing winner2
  p2Loop driver channel

-- | Manually negociate the connection between the two peers
-- | via an interface.
setupRemoteMenu :: forall e. RemoteDriver (err :: EXCEPTION | e ) -> TwoPlayerGameEffects e Unit
setupRemoteMenu driver = do
  let stateRef = driver.state
  el <- HA.selectElement (QuerySelector "#remote-setup")
  case el of
    (Just element) -> do
      liftEff $ HA.runHalogenAff do
        io <- runUI networkMenu unit element
        io.subscribe (consumer (\message -> do

          case message of
            (Connected connection channel playOrder) -> do
              showGame
              startGameState stateRef

              liftEff $ modifyRef stateRef (_ {
                connection = Just connection,
                channel = Just channel
              })

              void $ driver.io.query (H.action $ WinMenu.QSetDataChannel channel)

              let startGame =
                    case playOrder of
                      PlayFirst -> do
                        CR.runProcess (p1Loop driver channel)
                      PlaySecond -> do
                        CR.runProcess (p2Loop driver channel)

              driver.io.subscribe (consumer (\message' -> do
                case message' of
                  (WinMenu.PlayAgain) -> do
                    resetBoard stateRef
                    startGameState stateRef
                    void $ attempt startGame
                    pure $ Nothing
                  (WinMenu.MainMenu) -> do
                    resetBoard stateRef
                    closeConnection connection
                    pure $ Just unit
                  _ -> pure $ Nothing
              ))

              void $ forkAff $ attempt startGame

              pure $ Nothing
            (Disconnected m) -> do
              log "disconnected"
              showMessage "Your opponent has left the game."
              s <- liftEff $ readRef stateRef

              case s.gameinprogress of
                true -> driver.io.query (H.action $ WinMenu.QSetForfeitWin)
                false -> pure unit

              pure $ Nothing
        ))
    Nothing -> pure unit
