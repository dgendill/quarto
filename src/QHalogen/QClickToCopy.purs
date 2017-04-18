module QClickToCopy (
  Message(..),
  clickToCopy,
  Query(..)
  ) where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as HES
import Control.Monad.Aff (Aff, attempt, runAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import DOM (DOM)
import DOM.HTML.Types (HTMLElement)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn1, runFn1, runFn2)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

type State =
  { message :: Either String String
  , messageVisible :: Boolean
  , value :: String
  }

data Query a
  = Initialize a
  | CopySuccessful Boolean (HES.SubscribeStatus -> a)
  | HideMessage a

type Input = Unit

data Message
  = CopySuccess
  | CopyFailure String

type CAff eff = Aff (dom :: DOM, console :: CONSOLE, avar :: AVAR | eff)

successMessage :: String
successMessage = "This has been copied this to your clipboard."

failMessage :: String
failMessage = "Press Ctrl-C or ⌘-C to copy the text."

textarea :: H.RefLabel
textarea = H.RefLabel "textarea"

copyButton :: H.RefLabel
copyButton = H.RefLabel "copybutton"

foreign import clickToCopyElementValue_ :: forall e. Fn2 HTMLElement HTMLElement (Aff (dom :: DOM | e) Unit)

foreign import selectElementText_ :: forall e. Fn1 HTMLElement (Eff (dom :: DOM | e) Boolean)

selectElementText :: forall e. HTMLElement -> (Eff (dom :: DOM | e) Boolean)
selectElementText = runFn1 selectElementText_

clickToCopyElementValue :: forall e. HTMLElement -> HTMLElement -> (Aff (dom :: DOM | e) Unit)
clickToCopyElementValue = runFn2 clickToCopyElementValue_

clickToCopy :: forall eff. String -> H.Component HH.HTML Query Input Message (CAff eff)
clickToCopy value =
  H.lifecycleComponent
    { initialState: const initialState
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { message : Right successMessage
    , messageVisible : false
    , value : value
    }


  render :: State -> H.ComponentHTML Query
  render state =
    HH.div
      [ HP.class_ (H.ClassName "copy-text-box") ]
      [ HH.textarea [ HP.ref textarea, HP.readOnly true, HP.value state.value]
      , HH.div_
        -- [ HH.button [HP.ref copyButton, HE.onClick (HE.input_ Copy)] [ HH.text "Copy"]
        [ HH.button [HP.ref copyButton] [ HH.text "Copy"]
        , if state.messageVisible then showMessage else (HH.text "")
        ]
      ]
    where
    showMessage =
      case state.message of
        Right message -> HH.span [
          HP.class_ (H.ClassName "copy-text-box-success")
        ] [ HH.text message ]
        Left message -> HH.span [
          HP.class_ (H.ClassName "copy-text-box-fail")
        ]  [ HH.text message ]


  eval :: Query ~> H.ComponentDSL State Query Message (CAff eff)
  eval = case _ of
    Initialize next -> do
      H.getHTMLElementRef copyButton >>= (\mbutton -> do
        H.getHTMLElementRef textarea >>= (\mtextarea -> do
          case (Tuple mbutton mtextarea) of
            Tuple (Just button') (Just textarea') -> do

              H.subscribe $ H.eventSource (\callback -> do
                void $ (runAff
                  (handleCopyError textarea' callback)
                  (handleCopySuccess textarea' callback)
                  (attempt (clickToCopyElementValue button' textarea'))
                )
                pure unit
              ) (\wasSuccess -> do
                Just $ H.request $ CopySuccessful wasSuccess
              )
            _ -> pure unit
        )
      )
      pure next

    CopySuccessful wasSuccess reply -> do
      H.modify (_ {
        messageVisible = true,
        message = if wasSuccess then (Right successMessage) else (Left failMessage)
      })
      pure (reply HES.Listening)
    HideMessage next -> do
      H.modify (_ {
        messageVisible = false
      })
      pure next

handleCopyError :: forall e m a. (Bind m) => (MonadEff (dom :: DOM | e) m) => HTMLElement -> (Boolean -> Eff (dom :: DOM | e) Unit) -> a -> m Unit
handleCopyError textarea' callback err = do
  void $ H.liftEff (selectElementText textarea')
  H.liftEff $ callback false

handleCopySuccess :: forall e m a. (Bind m) => (MonadEff (dom :: DOM | e) m) => HTMLElement -> (Boolean -> Eff (dom :: DOM | e) Unit) -> a -> m Unit
handleCopySuccess textarea' callback success = do
  void $ H.liftEff (selectElementText textarea')
  H.liftEff $ callback true
