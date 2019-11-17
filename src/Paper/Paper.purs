module Paper (
    setup
) where

import Prelude
import Effect (Effect)

-- http://paperjs.org/tutorials/getting-started/using-javascript-directly/#making-the-scope-global
-- foreign import installOnWindow :: Effect Unit
foreign import setup :: String -> Effect Unit