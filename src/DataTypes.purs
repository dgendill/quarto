module DataTypes where

import Prelude
import Control.Alt (alt)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, readRef)
import DOM.Event.Event (Event)
import Data.Argonaut (class DecodeJson, class EncodeJson, JObject, Json, decodeJson, encodeJson, fromString, jsonEmptyObject, jsonParser, jsonSingletonObject, toString)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (getField)
import Data.Argonaut.Encode ((:=), (~>))
import Data.Either (Either(..))
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
import State (Board, PieceID, PositionID)
import WebRTC.RTC (RTCDataChannel, RTCPeerConnection)

-- | Who plays first and second in a game
data PlayOrder
  = PlayFirst
  | PlaySecond

-- | The types of games that we'll handle
data GameType
  = TwoPlayerSameTerminal
  | TwoPlayerRemote
  | SinglePlayer

-- | How we'll communicate between remote plays
data Protocol
  = PState { ondeck :: Maybe PieceID, board :: Board, gameinprogress :: Boolean }
  | PGive PieceID
  | PPlay PositionID
  | StartNewGame
  | OK

-- | A generic event
type BEvent a = { event :: Event | a }

-- | Events triggered by the pieces
type PieceEvent = BEvent (piece :: PieceID)

-- | Events triggered by the board
type BoardEvent = BEvent (position :: PositionID)

-- | The most generic game state
type BGameState a = {
  ondeck :: Maybe PieceID,
  board :: Board,
  gameinprogress :: Boolean,
  gametype :: GameType | a
}

-- | The single player game state
type GameState = BGameState ()

-- | The row of types that extend the base game
-- | state with the webrtc connection and data channel
type TwoPlayerGameStateExt = (
  connection :: Maybe RTCPeerConnection,
  channel :: Maybe RTCDataChannel
)

-- | The two player game state
type TwoPlayerGameState = BGameState TwoPlayerGameStateExt

derive instance gGameType  :: Generic GameType
instance showGameType :: Show GameType where show = gShow

instance encodeGameType :: EncodeJson GameType where
  encodeJson TwoPlayerSameTerminal = fromString "TwoPlayerSameTerminal"
  encodeJson TwoPlayerRemote = fromString "TwoPlayerRemote"
  encodeJson SinglePlayer = fromString "SinglePlayer"


instance decodeGameType :: DecodeJson GameType where
  decodeJson a = case toString a of
    Just "TwoPlayerSameTerminal" -> Right TwoPlayerSameTerminal
    Just "TwoPlayerRemote" -> Right TwoPlayerRemote
    Just "SinglePlayer" -> Right SinglePlayer
    _ -> Left "Bad Game Type"


instance showSend :: Show Protocol where
  show (PState gs) = "Send:Board:" <> (show gs.board) <> "|OnDeck:" <> (show gs.ondeck)
  show (PGive piece) = "Give:" <> piece
  show (PPlay position) = "Play:" <> position
  show OK = "OK"
  show StartNewGame = "StartNewGame"


instance encodeSendState :: EncodeJson Protocol where
  encodeJson (PState gs) =
    jsonSingletonObject "state"
      ("ondeck" := gs.ondeck
       ~> "board" := gs.board
       ~> "gameinprogress" := gs.gameinprogress
       ~> jsonEmptyObject
       )
  encodeJson (PGive pieceId) =
    ("action" := "give"
    ~> "piece" := pieceId
    ~> jsonEmptyObject)
  encodeJson OK =
    ("action" := "ok"
    ~> jsonEmptyObject)
  encodeJson StartNewGame =
    ("action" := "startnewgame"
    ~> jsonEmptyObject)
  encodeJson (PPlay positionId) =
    ("action" := "play"
    ~> "position" := positionId
    ~> jsonEmptyObject)


instance decodeSendStateInstance :: DecodeJson Protocol where
  decodeJson json = (decodePState json) `alt` (decodePGivePPlay json)


decodePState :: Json -> Either String Protocol
decodePState json = do
  obj <- decodeJson json
  state <- getField obj "state" :: Either String JObject
  board <- getField state "board" :: Either String Board
  ondeck <- getField state "ondeck" :: Either String (Maybe PieceID)
  gameinprogress <- getField state "gameinprogress" :: Either String Boolean
  pure $ PState { ondeck, board, gameinprogress }


decodePGivePPlay :: Json -> Either String Protocol
decodePGivePPlay json = do
  obj <- decodeJson json
  action <- getField obj "action" :: Either String String
  case action of
    "ok" -> do
      pure $ OK
    "give" -> do
      piece <- getField obj "piece" :: Either String PieceID
      pure $ PGive piece
    "play" -> do
      position <- getField obj "position" :: Either String PositionID
      pure $ PPlay position
    "startnewgame" -> do
      pure $ StartNewGame
    _ -> Left $ "Did not recognize action" <> action


encodeState :: TwoPlayerGameState -> String
encodeState p = encodeProtocol $
  PState { board : p.board, ondeck : p.ondeck, gameinprogress : p.gameinprogress }


encodeStateRef :: forall e. Ref (TwoPlayerGameState) -> Aff (ref :: REF | e) String
encodeStateRef r = (liftEff $ readRef r) >>= \p -> pure $ encodeProtocol $
  PState { board : p.board, ondeck : p.ondeck, gameinprogress : p.gameinprogress }


encodeGive :: PieceID -> String
encodeGive piece = encodeProtocol $ PGive piece


encodePlay :: PositionID -> String
encodePlay position = encodeProtocol $ PPlay position


encodeProtocol :: Protocol -> String
encodeProtocol = (stringify <<< encodeJson)


decodeProtocol :: String -> Either String Protocol
decodeProtocol string = do
  ss <- (jsonParser string)
  p <- (decodeJson ss :: Either String Protocol)
  pure p
