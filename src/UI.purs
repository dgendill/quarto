module UI (
    getEventType,
    onClick,
    onEvent
  ) where

import Prelude
import Control.Coroutine as CR
import Control.Coroutine (Consumer)
import Control.Monad.Eff.Ref (Ref)
import Control.Monad.Trans.Class (lift)
import DOM.Event.Event (Event, type_)
import Data.Newtype (unwrap)
import Effects (GameEffects)
import DataTypes (BEvent, BGameState)

-- | Return the event type of the
-- | Event.
getEventType :: forall a. BEvent a -> String
getEventType e = unwrap (type_ e.event)

-- | A consumer of "click" events that calls a callback
-- | when clicks happen.  The callback
-- | is passed the GameState and the Event.
onClick :: forall a b c e. Ref (BGameState b) -> (Ref (BGameState b) -> BEvent a -> (GameEffects e) c) -> Consumer { event :: Event | a } (GameEffects e) c
onClick = onEvent "click"

-- | A consumer of events that calls a callback
-- | when a particular event is consumed.  The callback
-- | is passed the GameState and the Event.
onEvent :: forall a b c e
   . String
  -> Ref (BGameState b)
  -> (Ref (BGameState b) -> BEvent a -> (GameEffects e) c)
  -> Consumer { event :: Event | a } (GameEffects e) c
onEvent name state callback = go
  where
  go = do
    e <- CR.await
    case (name == getEventType e) of
      true -> do
        lift $ callback state e
      false -> go
