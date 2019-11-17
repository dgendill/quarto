module Animation (
    animatePieceToHome,
    animatePieceToPosition,
    animatePieceToDeck,
    animatePieceToAbsPosition
  ) where

import Prelude

import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import State (PieceID, PositionID)

foreign import animatePieceToHome_ :: Fn1 PieceID (EffectFnAff Unit)

animatePieceToHome :: PieceID -> Aff Unit
animatePieceToHome = fromEffectFnAff <<< runFn1 animatePieceToHome_

foreign import animatePieceToPosition_ :: Fn2 PieceID PositionID (EffectFnAff Unit)

animatePieceToPosition :: PieceID -> PositionID -> Aff Unit
animatePieceToPosition a b = fromEffectFnAff $ runFn2 animatePieceToPosition_ a b

foreign import animatePieceToDeck_ :: Fn1 PieceID (EffectFnAff Unit)

animatePieceToDeck :: String -> Aff Unit
animatePieceToDeck = fromEffectFnAff <<< runFn1 animatePieceToDeck_

foreign import animatePieceToAbsPosition_ :: Fn3 PieceID Int Int (EffectFnAff Unit)

animatePieceToAbsPosition :: String -> Int -> Int -> Aff Unit
animatePieceToAbsPosition a b c = fromEffectFnAff $ runFn3 animatePieceToAbsPosition_ a b c
