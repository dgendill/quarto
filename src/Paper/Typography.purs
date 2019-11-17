module Paper.Typography (
    PointText,
    pointtext
) where

import Data.Function.Uncurried (Fn2, runFn2)
import Paper.Types (Point, Rectangle)
import Prim.Row (class Union)

foreign import data PointText :: Type

type PointTextConfig = (
    fillColor :: String,
    justification :: String,
    content :: String,

    position :: Point,
    pivot :: Point,
    bounds :: Rectangle,
    strokeBounds :: Rectangle,
    handleBounds :: Rectangle,
    internalBounds :: Rectangle,
    rotation :: Number,
    scaling :: Point,
    -- matrix
    -- globalMatrix
    -- viewMatrix
    -- applyMatrix

    fontSize :: String,
    visible :: Boolean,
    name :: String,

    fillColor :: String
)


pointtext :: forall option option'
             . Union option option' PointTextConfig
             => Point -> Record option -> PointText
pointtext point options = runFn2 pointtext_ point options


foreign import pointtext_ :: forall a. Fn2 Point (Record a) PointText


-- var text = new PointText(new Point(200, 50));
-- text.justification = 'center';
-- text.fillColor = 'black';
-- text.content = 'The contents of the point text';
