module GameLogic where

import Prelude

import Actions (givePiece, playPiece)
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Data.Maybe (Maybe(..))
import DataTypes (BGameState, BoardEvent, GameState, GameType(..), PieceEvent, Protocol(PState), TwoPlayerGameState, TwoPlayerGameStateExt)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref, modify_, read)
import Game (disableAvailablePieces, disableBoard)
import GameGraphics (hideMessage, itemName, listenToAvailablePieces, listenToBoard)
import State (Board, Piece, PieceID, PositionID, emptyBoard, isDraw, placePieceS, winningBoard)

updateState :: forall a.
  Ref (BGameState a) ->
  Protocol ->
  Aff Unit
updateState state (PState transferedState) = liftEffect $ modify_
  (\s -> s {
    board = transferedState.board,
    ondeck = transferedState.ondeck,
    gameinprogress = transferedState.gameinprogress
  }) state
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

resetBoard :: forall a. Ref (BGameState a) -> Aff Unit
resetBoard state = liftEffect (modify_ (_ { ondeck = i.ondeck, board = i.board }) state)
  where i = initialState

updateBoard :: forall a. Ref (BGameState a) -> Board -> Aff Unit
updateBoard state nboard = liftEffect (modify_ (_ { board = nboard }) state)

startGameState :: forall a. Ref {gameinprogress :: Boolean | a} -> Aff Unit
startGameState state = liftEffect $ modify_ (_ { gameinprogress = true } ) state

stopGame :: forall a. Ref {gameinprogress :: Boolean | a} -> Aff Unit
stopGame state = liftEffect $ modify_ (_ { gameinprogress = false } ) state

setOnDeck :: forall a. Ref { ondeck :: Maybe PieceID | a } ->  PieceID -> Aff Unit
setOnDeck state piece = liftEffect $ modify_ (_ { ondeck = (Just piece) }) state

twoPlayerSameTerminalGive :: forall a b. Ref (BGameState a) ->  { piece :: PieceID | b } -> Aff Unit
twoPlayerSameTerminalGive state e = do
  setOnDeck state e.piece
  disableAvailablePieces
  givePiece e.piece

twoPlayerSameTerminalPlay :: forall a b. Ref (BGameState a) -> { position :: PositionID | b} -> Aff Unit
twoPlayerSameTerminalPlay state evt = do
  s <- liftEffect $ read state
  case (s.ondeck) of
    Just (ondeck) -> do
      let nboard = placePieceS (s.board) ondeck (evt.position)
      updateBoard state nboard
      playPiece evt.position ondeck
      disableBoard
    _ -> pure unit


boardEventProducer :: CR.Producer BoardEvent Aff Unit
boardEventProducer = do
  CRA.produceAff (\emitter -> do
    listenToBoard (\i e -> do
      emit emitter ({ position : itemName i, event : e })
    )
  )

pieceProducer :: CR.Producer PieceEvent Aff Unit
pieceProducer =
  CRA.produceAff (\emitter -> do
    listenToAvailablePieces (\i e -> do
      emit emitter ({ piece : itemName i, event : e })
    )
  )

type WinHandler a =
  Ref (BGameState a) ->
  (Array Piece -> Aff Unit) ->
  Aff (Maybe (Array Piece))


type Driver a = {
  winHandler :: WinHandler a,
  state :: Ref (BGameState a)
}

type RemoteDriver e = Driver TwoPlayerGameStateExt


createDriver :: forall a.
  Ref (BGameState a) ->
  (WinHandler a) ->
  Driver a
createDriver state winHandler = { winHandler, state }


basicWinHandler :: forall a.
  Ref (BGameState a)
  -> Driver a
basicWinHandler state = createDriver state \_ onwin -> do
  s <- liftEffect $ read state

  let draw = isDraw s.board
  case draw of
    true -> do
      hideMessage
      stopGame state
      pure Nothing
    false -> do
      let mwinner = winningBoard s.board
      case mwinner of
        Nothing -> pure Nothing
        (Just winner) -> do
          onwin winner
          stopGame state
          pure $ Just winner
