module GameLogic where

import Prelude
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Halogen as H
import QHalogen.WinMenu as WinMenu
import Actions (givePiece, playPiece)
import Animation (animatePieceToDeck)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (Ref, modifyRef, readRef)
import Control.MonadPlus (guard)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effects (GameEffects)
import Game (disableAvailablePieces, disableBoard)
import GameGraphics (hideGivePieceText, hideMessage, hidePlayPieceText, itemName, listenToAvailablePieces, listenToBoard, newGame, showMessage)
import Halogen (HalogenIO)
import Menus (hideGame, showMainMenu)
import DataTypes (BGameState, BoardEvent, GameState, GameType(..), PieceEvent, Protocol(PState), TwoPlayerGameState, TwoPlayerGameStateExt)
import State (Board, Piece, PieceID, PositionID, emptyBoard, isDraw, placePieceS, winningBoard)
import WebRTC.RTC (closeConnection)

updateState :: forall a e.
  Ref (BGameState a) ->
  Protocol ->
  GameEffects e Unit
updateState state (PState transferedState) = liftEff $ modifyRef state
  (\s -> s {
    board = transferedState.board,
    ondeck = transferedState.ondeck,
    gameinprogress = transferedState.gameinprogress
  })
updateState _ _ = pure unit


initialState :: GameState
initialState = {
  ondeck : Nothing,
  board : emptyBoard,
  gameinprogress : false,
  gametype : TwoPlayerSameTerminal
}

initialTwoPlayerState :: TwoPlayerGameState
initialTwoPlayerState = {
  ondeck : Nothing,
  board : emptyBoard,
  gameinprogress : false,
  gametype : TwoPlayerRemote,
  connection : Nothing,
  channel : Nothing
}

-- endGame :: forall a e. Ref (BGameState a) -> GameEffects e Unit
-- endGame state = liftEff $ modifyRef state (_ { gameinprogress = false })

resetBoard :: forall a e. Ref (BGameState a) -> GameEffects e Unit
resetBoard state = liftEff (modifyRef state (_ { ondeck = i.ondeck, board = i.board }))
  where i = initialState

updateBoard :: forall a e. Ref (BGameState a) -> Board -> GameEffects e Unit
updateBoard state nboard = liftEff (modifyRef state (_ { board = nboard }))

startGameState :: forall a e. Ref {gameinprogress :: Boolean | a} -> GameEffects e Unit
startGameState state = liftEff $ modifyRef state (_ { gameinprogress = true } )

stopGame :: forall a e. Ref {gameinprogress :: Boolean | a} -> GameEffects e Unit
stopGame state = liftEff $ modifyRef state (_ { gameinprogress = false } )

setOnDeck :: forall a e. Ref { ondeck :: Maybe PieceID | a } ->  PieceID -> GameEffects e Unit
setOnDeck state piece = liftEff $ modifyRef state (_ { ondeck = (Just piece) })

twoPlayerSameTerminalGive :: forall a b e. Ref (BGameState a) ->  { piece :: PieceID | b } -> GameEffects e Unit
twoPlayerSameTerminalGive state e = do
  setOnDeck state e.piece
  disableAvailablePieces
  givePiece e.piece

twoPlayerSameTerminalPlay :: forall a b e. Ref (BGameState a) -> { position :: PositionID | b} -> GameEffects e Unit
twoPlayerSameTerminalPlay state evt = do
  s <- liftEff $ readRef state
  case (s.ondeck) of
    Just (ondeck) -> do
      let nboard = placePieceS (s.board) ondeck (evt.position)
      updateBoard state nboard
      playPiece evt.position ondeck
      disableBoard
    _ -> pure unit


boardEventProducer :: forall e. CR.Producer BoardEvent (GameEffects e) Unit
boardEventProducer = do
  CRA.produceAff (\emit -> do
    listenToBoard (\i e -> do
      emit (Left $ { position : itemName i, event : e })
    )
  )

pieceProducer :: forall e. CR.Producer PieceEvent (GameEffects e) Unit
pieceProducer =
  CRA.produceAff (\emit -> do
    listenToAvailablePieces (\i e -> do
      emit (Left $ { piece : itemName i, event : e } )
    )
  )

type WinHandler e a =
  (Ref (BGameState a) ->
  (Array Piece -> GameEffects e Unit) ->
  GameEffects e (Maybe (Array Piece)))

  -- ->
  --GameEffects e (Maybe (Array Piece)))

type Driver e a = {
  io :: HalogenIO WinMenu.Query WinMenu.Message (GameEffects e),
  winHandler :: WinHandler e a,
  state :: Ref (BGameState a)
}

type RemoteDriver e = Driver e TwoPlayerGameStateExt


createDriver :: forall e a.
  (HalogenIO WinMenu.Query WinMenu.Message (GameEffects e)) ->
  Ref (BGameState a) ->
  (WinHandler e a) ->
  Driver e a
createDriver io state winHandler = { io,  winHandler, state }


basicWinHandler :: forall e a
   . (HalogenIO WinMenu.Query WinMenu.Message (GameEffects e))
  -> Ref (BGameState a)
  -> Driver e a --GameEffects e (Maybe (Array Piece))
basicWinHandler io state = createDriver io state \_ onwin -> do
  s <- liftEff $ readRef state

  let draw = isDraw s.board
  case draw of
    true -> do
      hideMessage
      stopGame state
      void $ io.query (H.action WinMenu.QSetDraw)
    false -> pure unit

  guard $ draw == false

  let mwinner = winningBoard s.board
  case mwinner of
    Nothing -> pure Nothing
    (Just winner) -> do
      onwin winner
      stopGame state
      void $ io.query (H.action WinMenu.QShow)
      void $ io.query (H.action (WinMenu.QSetWinner winner))
      pure $ Just winner
