module Util (
    doNothing,
    randomElement,
    mapIndex,
    unindex,
    spanN,
    I4,
    indexArray4
  ) where

import Prelude
import Data.Array as Array
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Array (foldl, head, snoc, tail)
import Data.Maybe (Maybe(..))
import Data.Ord (greaterThan)
import Data.Tuple (Tuple(..), fst, snd)

-- | Alias for const $ pure unit
doNothing :: forall a b. (Applicative b) => a -> b Unit
doNothing = const $ pure unit

-- | Maybe get a random element from a set
randomElement :: forall e a. Array a -> Eff (random :: RANDOM | e) (Maybe a)
randomElement points
  | Array.length points == 0 = pure $ Nothing
  | otherwise = do
    i <- randomInt 0 ((Array.length points) - 1)
    pure $ Array.index points i

-- | Map a set to the same set but indexed so fst is the index and snd is
-- | element of the set.
mapIndex :: forall a. Array a -> (Array (Tuple Int a))
mapIndex = snd <<< (foldl (\(Tuple count a) val -> Tuple (count + 1) (snoc a (Tuple count val))) (Tuple 0 []))

-- | Remove the index created by mapIndex
unindex :: forall a. Array (Tuple Int a) -> Array a
unindex = map snd

-- | Splits an array into two parts:
-- | 1. the longest initial subarray for which all elements satisfy `index < n`
-- | 2. the remaining elements
spanN :: forall a. Int -> Array a -> { init :: Array a, rest :: Array a }
spanN n a = unindexSplit $ Array.span (fst >>> greaterThan n) (mapIndex a)
  where unindexSplit split = { init : unindex split.init, rest : unindex split.rest }

type I4 a = { i1 :: a, i2 :: a, i3 :: a, i4 :: a }

-- | Attempt to convert a set of elements into a
-- | record with 4 rows, where i1, i2, i3, and i4
-- | correspond to the first four elements of the array.
indexArray4 :: forall a. Array a -> Maybe (I4 a)
indexArray4 a = do
  -- a
  i1 <- head a
  -- Array a
  t1 <- tail a
  -- a
  i2 <- head t1
  -- Array a
  t2 <- tail t1
  -- a
  i3 <- head t2
  -- Array a
  t3 <- tail t2
  -- a
  i4 <- head t3
  -- Array a
  t4 <- tail t3

  pure $ {i1, i2, i3, i4}
