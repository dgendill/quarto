module QHalogen.NetworkMenu where

import Prelude
import Control.Monad.Eff.Console as Effc
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Aria
import Halogen.Query.EventSource as HES
import QClickToCopy as CTC
import Control.Monad.Aff (Aff, forkAff, delay, runAff)
import Control.Monad.Aff.AVar (AVAR, makeVar', modifyVar, peekVar)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import Data.Either (either)
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import DataTypes (PlayOrder(..))
import Halogen (ClassName(ClassName), HalogenM, PropName(PropName), eventSource, eventSource_, subscribe)
import Menus (showMainMenu)
import MyRTC (basicOffer, connection, createAnswerString, createOfferString, readDescription)
import QHalogen.HTML (verticalButtonList)
import WebRTC.RTC (RTC, RTCDataChannel, RTCPeerConnection, createAnswer, createDataChannel, oncloseChannel, ondataChannel, onicecandidate, setLocalDescription, setRemoteDescription)

type ProgressCurrent = Int
type ProgressMax = Int

data Stage
  = Start
  | Loading String ProgressCurrent ProgressMax
  | Failed String
  | CreateOffer String
  | EnterAnswer String String
  | EnterOffer String
  | CreateAnswer String
  | IsConnected


type State =
  { stage :: Stage
  , connection :: Maybe RTCPeerConnection
  , channel :: Maybe RTCDataChannel
  , playOrder :: Maybe PlayOrder
  , visible :: Boolean
  }

data Query a
  = QCreateOffer a
  | QShowCreatedOffer String a
  | QEnterAnswer String String a
  | QValidateAnswer String a
  | QEnterOffer String a
  | QValidateOffer String a
  | QUpdateLoading String ProgressCurrent ProgressMax (HES.SubscribeStatus -> a)
  | JumpToStage Stage a
  | QConnect a
  | QShowConnected RTCDataChannel (HES.SubscribeStatus -> a)
  | QHideNetworkMenu a
  | QRaiseDisconnect (HES.SubscribeStatus -> a)
  | QCopyText CTC.Message a


type Input = Unit

data Message
  = Connected RTCPeerConnection RTCDataChannel PlayOrder
  | Disconnected String

maybee :: forall a b. (a -> b) -> b -> (Maybe a) -> b
maybee f d m = fromMaybe d (f <$> m)

type CAff eff = Aff (dom :: DOM, rtc :: RTC, console :: CONSOLE, avar :: AVAR | eff)

wrapperProps state =
  if state.visible
    then [Aria.hidden "false", HP.class_ (ClassName "inner"), HP.id_ "remote-setup-menu"]
    else [Aria.hidden "true", HP.classes [(ClassName "hidden"), (ClassName "inner")], HP.id_ "remote-setup-menu"]

data Slot = CopyTextareaSlot
derive instance eqCopyTextareaSlot :: Eq Slot
derive instance ordCopyTextareaSlot :: Ord Slot

networkMenu :: forall e. H.Component HH.HTML Query Input Message (CAff e)
networkMenu =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { stage : Start
    , connection : Nothing
    , channel : Nothing
    , playOrder : Nothing
    , visible : true
    }

  render :: State -> H.ParentHTML Query CTC.Query Slot (CAff e)
  render state =
    HH.div (wrapperProps state)
      [ HH.h1 [HP.class_ (ClassName "title")] [HH.text "Quarto Legends"]
      , HH.div_
        case state.stage of
        Start -> [
          verticalButtonList
            [ HH.button
              [ HP.class_ (ClassName "primary")
              , HE.onClick (HE.input_ QHideNetworkMenu)]
              [ (HH.span_ [HH.text "Back"]), (HH.small_ [HH.text "Back to main menu"]) ]
            , HH.button
              [ HP.class_ (ClassName "primary")
              , HE.onClick (HE.input_ QCreateOffer)]
              [ (HH.span_ [HH.text "Create Room"]), (HH.small_ [HH.text "Create a room token you can use to invite a friend."]) ]
            , HH.button
              [ HP.class_ (ClassName "primary")
              , HE.onClick (HE.input_ $ QEnterOffer "")]
              [ (HH.span_ [HH.text "Join Room"]), (HH.small_ [HH.text "Join a room with a token you've been sent."]) ]
            ]
          ]
        Loading message value max -> [
            HH.progress [HP.prop (PropName "max") max, HP.prop (PropName "value") value] [],
            HH.h3_ [HH.text message]

          ]
        CreateOffer offer ->
          [ HH.h3_ [
              HH.text "The room has been created. Copy the following invitation and send it to your opponent."
            ]
          , HH.slot CopyTextareaSlot (CTC.clickToCopy offer) unit (HE.input QCopyText)
          , verticalButtonList
              [ HH.button
                [ HP.class_ (ClassName "primary")
                , HE.onClick (HE.input_ $ JumpToStage Start)
                ]
                [HH.text "Go Back"]
              , HH.button
                [ HP.class_ (ClassName "primary")
                , HE.onClick (HE.input_ (QEnterAnswer "" offer))
                ]
                [HH.text "Ok, I sent it."]
              ]
          ]
        EnterAnswer answer offer -> [
          HH.h3_ [
            HH.text "Now your opponent should send you a response to the invitation.  Enter it here."
          ],
          HH.textarea [
            HP.class_ (ClassName "network-menu-textarea"),
            HP.value answer,
            HE.onValueInput (HE.input (\v -> QEnterAnswer v offer))

          ],
          verticalButtonList
            [ HH.button
              [ HP.class_ (ClassName "primary")
              , HE.onClick (HE.input_ $ QShowCreatedOffer offer)
              ]
              [ HH.text "Go Back" ]
            , HH.button
              [ HP.class_ (ClassName "primary")
              , HP.disabled (answer == "")
              , HE.onClick (HE.input_ $ QValidateAnswer answer)
              ]
              [ HH.text "Ok, I entered it." ]
            ]

        ]
        EnterOffer response -> [
            HH.h3_ [
              HH.text "Enter the invitation your partner sent you."
            ],
            HH.textarea [
             HP.class_ (ClassName "network-menu-textarea"),
             HE.onValueInput (HE.input QEnterOffer)
            ],
            verticalButtonList
              [ HH.button
                [ HP.class_ (ClassName "primary")
                , HE.onClick (HE.input_ $ JumpToStage Start)
                ]
                [ HH.text "Go Back" ]
              , HH.button
                [ HP.class_ (ClassName "primary")
                , HP.disabled (response == "")
                , HE.onClick (HE.input_ $ QValidateOffer response)
                ]
                [HH.text "Ok, I entered it."]
              ]
          ]
        CreateAnswer answer -> [
            HH.h3_ [ HH.text "Now send this invitation to your partner."],
            HH.slot CopyTextareaSlot (CTC.clickToCopy answer) unit (HE.input QCopyText),
            --HH.textarea [HP.class_ (ClassName "network-menu-textarea"), HP.value answer],
            verticalButtonList
              [ HH.button
                [ HP.class_ (ClassName "primary")
                , HE.onClick (HE.input_ $ QConnect)
                ]
                [HH.text "Ok, I sent it."]
              ]
          ]
        IsConnected -> [ HH.h3_ [ HH.text "Connection has been made."] ]
        Failed reason -> [
          HH.div [HP.class_ (ClassName "network-failure")] [
            HH.h3_ [ HH.text "Sorry! We can't seem to connect you."],
            HH.p_ [ HH.text "Possible reasons:"],
            HH.ol_ [
              HH.li_ [HH.text "Your device doesn't support WebRTC."],
              HH.li_ [HH.text "The connection needs a TURN server to negotiate the connection."],
              HH.li_ [HH.text "Google's STUN server isn't working."],
              HH.li_ [HH.text "The token was entered incorrectly."],
              HH.li_ [HH.text "Some other unforeseen reason."]
            ],
            HH.p_ [
              HH.text "In any case, you should probably just play by yourself or find a friend to sit next to you."
            ],
            verticalButtonList
              [ HH.button
                [ HP.class_ (ClassName "primary")
                , HE.onClick (HE.input_ QHideNetworkMenu)]
                [ (HH.span_ [HH.text "Main Menu"]), (HH.small_ [HH.text "Try two player or play against computer."]) ]
              ],
            HH.hr_,
            HH.p_ [ HH.text "The actual errors was: "],
            HH.blockquote_ [ HH.text reason ]
          ]
        ]
      ]

  eval :: Query ~> H.ParentDSL State Query CTC.Query Slot Message (CAff e)
  eval = case _ of
    QCopyText _ next ->
      pure next
    QHideNetworkMenu next -> do
      H.modify (_ { visible = false})
      liftAff $ showMainMenu
      pure next
    QUpdateLoading message time max reply -> do
      stage <- H.gets _.stage
      case stage of
        (Loading m t mt) -> do
          H.modify (_ { stage = Loading message time max })
          case time < max of
            true -> pure (reply H.Listening)
            false -> pure (reply H.Done)
        _ -> pure (reply H.Done)

    QCreateOffer next -> do
      let message = "Please wait while we create the room"
      updateLoading 10000 message

      c <- liftEff $ connection

      subscribe $ eventSource (\f -> do
        void $ runAff Effc.logShow (const $ pure unit) (forkAff do
          channel1 <- createDataChannel "quarto" c
          liftEff $ f channel1
        )) (\channel -> Just $ H.request (QShowConnected channel))

      offerDesc <- liftAff $ basicOffer c
      liftAff $ setLocalDescription offerDesc c

      offer <- liftAff $ createOfferString =<< onicecandidate c (\e -> do
        pure unit
      )

      H.modify (_ {
        stage = CreateOffer offer,
        connection = Just c,
        playOrder = Just PlayFirst
      })
      pure next
    QShowCreatedOffer offer next -> do
      H.modify (_ { stage = CreateOffer offer })
      pure next

    QEnterOffer response next -> do
      H.modify (_ { stage = EnterOffer response })
      pure next
    QRaiseDisconnect reply -> do
      H.raise (Disconnected "onclose event on the data channel fired.")
      pure (reply H.Done)
    QShowConnected channel reply -> do
      state <- H.get
      traverse_ H.raise (Connected <$> state.connection <*> (Just channel) <*> state.playOrder)

      subscribe $ eventSource_ (\eff -> do
        void $ runAff Effc.logShow (\a -> do
          eff
          pure unit
        ) (oncloseChannel channel (void $ forkAff $ do
          liftEff $ eff
      ))) (H.request QRaiseDisconnect)

      H.modify (_ { visible = false })

      pure (reply H.Done)
    QValidateOffer offer next -> do

      c <- liftEff $ connection

      subscribe $ eventSource (\f -> do
        void $ runAff Effc.logShow (const $ pure unit) (forkAff do
          channel1 <- ondataChannel c
          liftEff $ f channel1
          log "Joiner data channel open"
        )) (\channel -> Just $ H.request (QShowConnected channel))


      eDesc <- liftAff $ readDescription offer
      either
        (\err -> do
          H.modify (_ { stage = Failed err })
          pure unit
        )
        (\desc -> do
          liftAff $ setRemoteDescription desc c
          answerDesc <- liftAff $ createAnswer c
          liftAff $ setLocalDescription answerDesc c

          answer <- liftAff $ createAnswerString =<< onicecandidate c (\evt -> do
              log "ICE Validate Offer"
              pure unit
            )

          H.modify (_ {
            stage = CreateAnswer answer,
            connection = Just c,
            playOrder = Just PlaySecond
          })
          pure unit
        )
        eDesc
      pure next
    QEnterAnswer answer offer next -> do
      H.modify (_ { stage = EnterAnswer answer offer })
      pure next
    QValidateAnswer answer next -> do
      let message = "Please wait while we validate the answer."
      updateLoading 20000 message

      s <- H.get

      case s.connection of
        (Just connection) -> do

          eDesc <- liftAff $ readDescription answer
          either
            (\err -> do
              H.modify (_ { stage = Failed err })
              pure unit
            )
            (\desc -> do
              case s.connection of
                (Just c) -> do
                  liftAff $ setRemoteDescription desc c
                  pure unit
                Nothing -> pure unit

            )
            eDesc
        _ -> pure unit

      pure next


    JumpToStage newStage next -> do
      H.modify (_ { stage = newStage })
      pure next
    QConnect next -> do
      let message = "Please wait while we connect to your opponent."
      updateLoading 20000 message
      pure next

updateLoading :: forall s m r t u e.
  (MonadAff ( avar :: AVAR, console :: CONSOLE | e) m) =>
  Int -> String -> HalogenM { stage :: Stage | s } Query u t r m Unit
updateLoading max message = do
  H.modify (_ { stage = Loading message 0 max  })
  subscribe $ H.eventSource (forEveryUntil 500 max) (\time -> do
    Just $ H.request $ QUpdateLoading message time max
  )

-- | Continuously run a callback function ever `interval` milliseconds
-- | until the maximum time `remaining` is <= 0
runForTime :: forall e. Int -> Int -> Aff e Unit -> Aff e Unit
runForTime interval remaining callback
  | remaining <= 0 = pure unit
  | otherwise = do
      delay (Milliseconds $ toNumber interval)
      callback
      runForTime interval (remaining - interval) callback

-- | Every every `interval` milliseconds, run callback function `f`,
-- | until `max` milliseconds has ellapsed (approximately).
forEveryUntil :: forall e. Int -> Int -> (Int -> Eff (avar :: AVAR, console :: CONSOLE | e) Unit) -> Eff (avar :: AVAR, console :: CONSOLE | e) Unit
forEveryUntil interval max callback = do
  void $ runAff Effc.logShow (const $ pure unit) do
    time <- liftAff $ makeVar' 0
    (runForTime interval max do
      modifyVar (\v -> v + interval) time
      val <- peekVar time
      liftEff $ callback val
    )
