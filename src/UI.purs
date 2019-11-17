module UI (
    getEventType,
    onClick,
    onEvent
  ) where

import Prelude

import Control.Coroutine (Consumer)
import Control.Coroutine as CR
import Control.Monad.Trans.Class (lift)
import Data.Newtype (unwrap)
import DataTypes (BEvent, BGameState)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Web.Event.Event (type_)
import Web.Event.Internal.Types (Event)

-- | Return the event type of the
-- | Event.
getEventType :: forall a. BEvent a -> String
getEventType e = unwrap (type_ e.event)

-- | A consumer of "click" events that calls a callback
-- | when clicks happen.  The callback
-- | is passed the GameState and the Event.
onClick :: forall a b c
   . Ref (BGameState b)
  -> (Ref (BGameState b) -> BEvent a -> Aff c)
  -> Consumer { event :: Event | a } Aff c
onClick = onEvent "click"

-- | A consumer of events that calls a callback
-- | when a particular event is consumed.  The callback
-- | is passed the GameState and the Event.
onEvent :: forall a b c
   . String
  -> Ref (BGameState b)
  -> (Ref (BGameState b) -> BEvent a -> Aff c)
  -> Consumer { event :: Event | a } Aff c
onEvent name state callback = go
  where
  go = do
    e <- CR.await
    case (name == getEventType e) of
      true -> do
        lift $ callback state e
      false -> go
