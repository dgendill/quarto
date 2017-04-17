module QHalogen.WinMenu where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Aria
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import DOM.HTML.Types (readHTMLCanvasElement)
import Data.Array (catMaybes)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..), isJust)
import GameGraphics (GRAPHICS, PaperItem, PaperProject, PivotPosition(BottomCenter), addMiniPiecesTo, getPaperItem, getPaperProject, hideMessage, hidePaperItem, movePaperItemPivot, newGame, showPaperItem)
import Halogen (ClassName(ClassName))
import Menus (hideGame, showMainMenu)
import DataTypes (Protocol(..), decodeProtocol, encodeProtocol)
import QHalogen.HTML (verticalButtonList)
import State (Piece, miniPieceId)
import Util (I4, indexArray4)
import WebRTC.RTC (RTC, RTCDataChannel, onmessageChannelOnce, send)

awaitPeerData :: forall e. RTCDataChannel -> CAff e (Either String Protocol)
awaitPeerData channel = (onmessageChannelOnce channel) >>= decodeProtocol >>> pure

data EndGame
  = Draw
  | ForfeitWin
  | ForfeitLose
  | Win (I4 PaperItem)


type State =
  { paper :: Maybe PaperProject
  , visible :: Boolean
  , ending :: Maybe EndGame
  , awaitNewGameAgreement :: Boolean
  , channel :: Maybe RTCDataChannel
  }


data Query a
  = Initialize a
  | Finalize a
  | QShow a
  | QHide a
  | QSetWinner (Array Piece) a
  | QSetDraw a
  | QClearEnding a
  | QPlayAgain a
  | QMainMenu a
  | QAwaitNewGameAgreement a
  | QSetForfeitLose a
  | QSetForfeitWin a
  | QSetDataChannel RTCDataChannel a


type Input = Unit

data Message
  = PlayAgain
  | MainMenu
  | Winner (Array Piece)

type CAff eff = Aff (rtc :: RTC, graphics :: GRAPHICS, console :: CONSOLE, avar :: AVAR | eff)

wrapperProps state = case state.visible of
  true -> [Aria.hidden "false", HP.classes [H.ClassName "inner"]]
  false -> [Aria.hidden "true", HP.classes [ClassName "hidden", H.ClassName "inner"]]

initialState :: State
initialState =
  { paper : Nothing
  , visible : false
  , ending : Nothing
  , awaitNewGameAgreement : false
  , channel : Nothing
  }

remotePlayAgainButton :: State -> H.ComponentHTML Query
remotePlayAgainButton state = case state.awaitNewGameAgreement of
  true -> HH.p_ [HH.text "Waiting for opponent to click Play Again"]
  false ->
    HH.button
    [ HP.class_ (H.ClassName "play-again primary primary-small")
    , HE.onClick (HE.input_ $
        case state.channel of
          Just _ -> QAwaitNewGameAgreement
          _ -> QPlayAgain
        )]
    [ (HH.span_ [HH.text "Play Again"])]

render :: State -> H.ComponentHTML Query
render state =
    HH.div (wrapperProps state)
      [
      case state.ending of
        (Just (Win a)) ->
          HH.h3_ [HH.text "Quarto!"]
        (Just Draw) ->
          HH.h3_ [HH.text "Draw!"]
        (Just ForfeitWin) ->
          HH.div_ [
            HH.h3_ [HH.text "You win!"],
            HH.p_ [HH.text "Your cowardly opponent has forfeited."]
          ]
        (Just ForfeitLose) ->
          HH.div_ [
            HH.h3_ [HH.text "You lose."],
            HH.p_ [HH.text "Only cowards forfeit in quarto!"]
          ]
        _ -> HH. h3_ [HH.text ""]
      ,
      verticalButtonList
        [ remotePlayAgainButton state
        , HH.button
          [ HP.class_ (H.ClassName "main-menu primary primary-small")
          , HE.onClick (HE.input_ $ QMainMenu)]
          [ (HH.span_ [HH.text "Main Menu"])]
        ]
      , HH.canvas [ HP.ref canvasRef, HP.class_ (H.ClassName "win-screen-canvas"), HP.width 300, HP.height 300 ]
      ]

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (CAff eff)
eval = case _ of
  Initialize next -> do

    H.getHTMLElementRef canvasRef >>=
      traverse_ (toForeign >>> readHTMLCanvasElement >>> runExcept >>> (either (const $ pure unit) (\canvas -> do
        liftAff $ log "canvas exists"
        let project = getPaperProject canvas
        liftAff $ addMiniPiecesTo project
        H.modify (_ { paper = Just project })
      )))

    pure next
  Finalize next -> do
    pure next
  QShow next -> do
    H.modify (_ { visible = true } )
    pure next
  QHide next -> do
    H.modify (_ { visible = false } )
    pure next
  QSetDataChannel channel next -> do
    H.modify (_ { channel = Just channel })
    pure next
  QAwaitNewGameAgreement next -> do
    H.gets _.channel >>= case _ of
      Just channel -> do

        liftEff $ send (encodeProtocol StartNewGame) channel
        H.modify (_ { awaitNewGameAgreement = true })
        response1 <- liftAff $ awaitPeerData channel
        case response1 of
          (Right OK) -> do
            H.modify (_ { awaitNewGameAgreement = false })
            eval (H.action QPlayAgain)
            pure unit
          (Right StartNewGame) -> do
            liftEff $ send (encodeProtocol OK) channel
            H.modify (_ { awaitNewGameAgreement = false })
            eval (H.action QPlayAgain)
            pure unit
          (Right e) -> do
            liftAff $ log $ "Bad right" <> (show e)
          (Left e) -> do
            liftAff $ log e
            H.modify (_ { awaitNewGameAgreement = false })
        pure next
      _ -> pure next
  QSetWinner set next -> do
    liftAff $ log "QSetWinner"
    liftAff $ log $ show set

    s <- H.get

    liftAff $ log $ show (isJust s.paper)

    traverse_ (\p -> do
      let ids = map miniPieceId set

      -- Array PaperItem
      let items = catMaybes $ map (getPaperItem p) ids

      -- Maybe { i1, i2, i3, i4 }
      let indexed = indexArray4 items

      traverse_ (\i -> do
        H.modify (_ { ending = Just (Win i) } )
        liftAff $ do
          traverse_ showPaperItem [i.i1, i.i2, i.i3, i.i4]
          movePaperItemPivot BottomCenter 40 70 i.i1
          movePaperItemPivot BottomCenter 80 70 i.i2
          movePaperItemPivot BottomCenter 120 70 i.i3
          movePaperItemPivot BottomCenter 160 70 i.i4
      ) indexed

    ) s.paper

    H.raise (Winner set)

    pure next
  QSetDraw next -> do
    H.modify (_ {
      ending = Just Draw,
      visible = true
    })
    pure next
  QSetForfeitWin next -> do
    H.modify (_ {
      ending = Just ForfeitWin,
      visible = true
    } )
    pure next
  QSetForfeitLose next -> do
    H.modify (_ {
      ending = Just ForfeitLose,
      visible = true
    })
    pure next
  QClearEnding next -> do
    H.modify (_ { ending = Nothing } )
    pure next
  QPlayAgain next -> do
    hideWinner =<< H.get
    liftAff $ newGame
    liftAff $ hideMessage
    H.modify (_ {
      ending = Nothing,
      visible = false
    })
    H.raise PlayAgain
    pure next
  QMainMenu next -> do
    H.modify (_ {
      ending = Nothing,
      visible = false,
      channel = Nothing
    })
    liftAff $ newGame
    liftAff $ hideMessage
    liftAff $ showMainMenu
    liftAff $ hideGame
    H.raise MainMenu
    pure next

winMenu :: forall eff. H.Component HH.HTML Query Input Message (CAff eff)
winMenu = do
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Just (H.action Finalize)
    , receiver: const Nothing
    }

canvasRef :: H.RefLabel
canvasRef = H.RefLabel "menuCanvas"

hideWinner :: forall e. State -> H.ComponentDSL State Query Message (CAff e) Unit
hideWinner state =
  traverse_ (\p -> do
    traverse_ (\ending -> do
      case ending of
        (Win i) -> liftAff $ do
          traverse_ hidePaperItem [i.i1, i.i2, i.i3, i.i4]
        _ -> pure unit
    ) state.ending
  ) state.paper
