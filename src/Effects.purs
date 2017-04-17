module Effects where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import GameGraphics (GRAPHICS)
import WebRTC.RTC (RTC)

-- | Alias for all the effects the
-- | game needs.
type GameEffects e = (Aff (random :: RANDOM, exception :: EXCEPTION, rtc :: RTC, graphics :: GRAPHICS, avar :: AVAR, ref :: REF, console :: CONSOLE, dom :: DOM | e))
