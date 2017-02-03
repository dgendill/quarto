module Main.Test where

import Prelude
import State
import Data.Tuple
import Data.Maybe
import Data.Either
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Map (fromFoldable)
import Data.Array (length)

main :: forall e. Eff (console :: CONSOLE) Unit
main = do

  -- logShow makePieces

  logShow $ winningBoard $ fromFoldable
    [ Tuple (Tuple 1 1) (Just $ piece Tall White Hollow Square)
    , Tuple (Tuple 2 2) (Just $ piece Short White Hollow Square)
    , Tuple (Tuple 3 3) (Just $ piece Short White Solid Square)
    --, Tuple (Tuple 3 3) Nothing
    , Tuple (Tuple 4 4) (Just $ piece Tall White Hollow Circle)
    , Tuple (Tuple 2 1) (Just $ piece Short White Hollow Circle)
    , Tuple (Tuple 3 1) (Just $ piece Tall White Solid Circle)
    , Tuple (Tuple 4 1) (Just $ piece Short White Solid Circle)
  ]
  -- log $ "Length: " <> (show $ length makePieces)

  --let p = (piece Hollow Tall White Square)
  --logShow $ pieceClasses p
  -- case p of
  --   Right a -> log $ pieceClasses a
  --   Left b -> log b

  --p2 <- (piece Hollow Tall White Circle)
  --log $ pieceClasses <$> p2
