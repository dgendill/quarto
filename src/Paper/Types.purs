module Paper.Types (
    Size,
    size,
    Point,
    point,
    Rectangle,
    rectangle
) where

import Data.Function.Uncurried (Fn2, runFn2)
import Paper.Styling as Styling

foreign import data Size :: Type
foreign import size_ :: Fn2 Number Number Size
size :: Number -> Number -> Size
size = runFn2 size_

foreign import data Point :: Type
foreign import point_ :: Fn2 Number Number Point
point :: Number -> Number -> Point
point = runFn2 point_

foreign import data Rectangle :: Type
foreign import rectangle_ :: Fn2 Point Size Rectangle
rectangle :: Point -> Size -> Rectangle
rectangle = runFn2 rectangle_
