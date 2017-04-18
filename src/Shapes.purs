-- | Module for working with custom
-- | PaperJS graphic components
module Shapes (
    tooltip,
    labelArrow,
    removeGraphicItem,
    removeGraphicItemByName,
    Tooltip,
    LabelArrow,
    GraphicItem
  ) where

import Prelude (Unit)
import GameGraphics (AffGraphics)

foreign import data GraphicItem :: Type

type Tooltip =
  { name :: String
  , x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  , content :: String }

type LabelArrow =
  { name :: String
  , x :: Int
  , y :: Int
  , content :: String
  , width :: Int }

foreign import tooltip :: forall e. Tooltip -> AffGraphics e GraphicItem

foreign import labelArrow :: forall e. LabelArrow -> AffGraphics e GraphicItem

foreign import removeGraphicItem :: forall e. GraphicItem -> AffGraphics e Unit

foreign import removeGraphicItemByName :: forall e. String -> AffGraphics e Unit
