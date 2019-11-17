module Paper.Shape (
    Circle,
    circle,
    RectangleShape,
    rectangleShape
) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Paper.Styling (class PaperStyling, genericStyle) as Styling
import Paper.Types (Point, Size)

foreign import data RectangleShape :: Type
foreign import rectangleShape_ :: Fn2 Point Size RectangleShape
rectangleShape :: Point -> Size -> RectangleShape
rectangleShape = runFn2 rectangleShape_
instance rectStyle :: Styling.PaperStyling RectangleShape where
    style :: RectangleShape -> _ -> Effect RectangleShape
    style = Styling.genericStyle
instance discardRect :: Discard RectangleShape where
    discard = bind

foreign import data Circle :: Type
foreign import circle_ :: Fn2 Point Number Circle
circle :: Point -> Number -> Circle
circle = runFn2 circle_
instance circStyle :: Styling.PaperStyling Circle where
    style = Styling.genericStyle
instance discardCirc :: Discard Circle where
    discard = bind