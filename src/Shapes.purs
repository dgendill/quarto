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

import Prelude
import Data.Function.Uncurried (Fn1, runFn1)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Prelude (Unit)

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

foreign import tooltip_ :: Fn1 Tooltip (EffectFnAff GraphicItem)
tooltip tt = fromEffectFnAff $ runFn1 tooltip_ tt

foreign import labelArrow_ :: Fn1 LabelArrow (EffectFnAff GraphicItem)
labelArrow la =  fromEffectFnAff $ runFn1 labelArrow_ la

foreign import removeGraphicItem_ :: Fn1 GraphicItem (EffectFnAff Unit)
removeGraphicItem gi = fromEffectFnAff $ runFn1 removeGraphicItem_ gi

foreign import removeGraphicItemByName_ :: Fn1 String (EffectFnAff Unit)
removeGraphicItemByName gin = fromEffectFnAff $ runFn1 removeGraphicItemByName_ gin
