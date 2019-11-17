module Paper.Styling (
    fillColor,
    class PaperStyling,
    style,
    genericStyle
) where

import Prelude
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Prim.Row (class Cons, class Union)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.History (back)

type Style = (
    fillColor :: String,
    justification :: String
)

type StyleOption = {
    fillColor :: String,
    justification :: String
}

class PaperStyling a where
    style :: forall option option'
            . Union option option' Style
            => a -> Record option -> Effect a
    

foreign import fillColor_ :: forall a. Fn2 a String a
fillColor :: forall a. (PaperStyling a) => a -> String -> a
fillColor = runFn2 fillColor_

foreign import style_ :: forall a b. Fn2 a (Record b) a


-- genericStyle :: forall a option option'
--      . Union option option' Style
--     => a -> Record option -> Effect a
genericStyle a b = pure $ runFn2 style_ a b

-- makeStyle :: forall option option'
--              . Union option option' Style
--              => (Record option) -> StyleOption
-- makeStyle = unsafeCoerce