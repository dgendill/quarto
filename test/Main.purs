module Test.Main where

import Prelude
import Demo
import State
import Control.Monad.Eff.Console as EC
import Test.Unit.Assert as Assert
import AI (aiGetPlays, bestPlay, choosePlay, getBoard, hasAtLeastXTriples, hasMoreTriplesThan, hasTriples, isWinner)
import AffParallel (animatePieceToHome')
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, logShow)
import Control.Monad.Except (runExcept)
import Control.Monad.Free (foldFree, hoistFree, liftF, runFree)
import Data.Array (length)
import Data.Either (Either(..), isLeft)
import Data.Foldable (foldMap, traverse_)
import Data.Foreign (toForeign)
import Data.Foreign.Class (read)
import Data.Functor (voidLeft, voidRight)
import Data.Functor.Compose (Compose(..))
import Data.Generic.Rep (Sum)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (NaturalTransformation)
import Data.StrMap (fromFoldable)
import Data.Tuple (Tuple(..))
import Math (pow)
import Test.Unit (suite, test, timeout)
import Test.Unit.Main (runTest)
import Util (mapIndex, randomElement, spanN, unindex)


-- | An arbirary piece used for testing purposes
testPiece :: Piece
testPiece = piece Light Tall Circle Hollow

-- instance eqInitRest :: (Eq a) => Eq { init :: a, rest :: a } where
--   eq { init : a1, rest : b1 } { init : a2, rest : b2 } = a1 == a2 && b1 == b2

main :: Eff _ Unit
main = do
  log $ showBoard twoTriples
  nl
  play <- ((bestPlay twoTriples duelTwoTripleFinisher) >>= (pure <<< map getBoard))
  traverse_ (log <<< (showBoard)) play

  runTest $ do
    suite "State" do
      test "Foreign Point" do
        let a = toForeign "1,1"
        let b = toForeign "1"

        let ar = runExcept $ read a
        let br = ((runExcept $ read b) :: Either _ Point)

        Assert.assert ("1,1 was not parsed to point ") $ ar == (Right (mkPoint 1 1))
        Assert.assert ("1 was parsed but it's invalid ") $ (isLeft br) == true

      test "foldMapEmptySpaces" do
        let a = length $ foldMapEmptySpaces (const [1]) emptyBoard
        Assert.assert ("The empty board should have 16, but it was " <> show a) $ a == 16

      test "countEmptySpaces" do
        let c = countEmptySpaces emptyBoard
        let c2 = countEmptySpaces fullBoardButInvalid

        Assert.assert ("The empty board should have 16, but it was " <> show c) $ c == 16
        Assert.assert ("The full board should have 0, but it was " <> show c2) $ c2 == 0

      test "triples" do
        Assert.assert "All plays with same piece = 40 threes" $ (countTriples fullBoardButInvalid) == 40
        Assert.assert "Two threes = 2 threes " $ (countTriples twoTriples) == 2

    suite "Util" do
      test "randomElement" do
        a <- liftEff $ randomElement [1]
        b <- liftEff $ randomElement [] :: Eff _ (Maybe Int)
        c <- liftEff $ randomElement [1,2]
        Assert.assert "" $ a == Just 1
        Assert.assert "" $ b == Nothing
        Assert.assert "" $ ((c == Just 1) || (c == Just 2))

      test "mapIndex" do
        Assert.assert "" $ (mapIndex [1,2,3]) == [Tuple 0 1, Tuple 1 2, Tuple 2 3]
      test "unindex" do
        Assert.assert "" $ (unindex $ mapIndex [1,2,3]) == [1,2,3]
      test "spanN" do
        Assert.assert "" $ do
          let v = (spanN 2 [1,2,3,4])
          v.init == [1,2] && v.rest == [3,4]

nl = log "\n"

fullBoardButInvalid = foldMap (placePieceUnsafe emptyBoard (piece Light Tall Square Hollow)) makePoints

duelTwoTripleFinisher = piece Light Tall Circle Hollow
duelTwoTripleOther = piece Dark Short Circle Hollow

twoTriples = fromFoldable $
  [ Tuple "1,1" (Just $ piece Light Tall Square Hollow)
  , Tuple "2,2" (Just $ piece Light Short Square Hollow)
  , Tuple "4,4" (Just $ piece Light Short Square Solid)
  , Tuple "1,2" (Just $ piece Light Short Circle Solid)
  , Tuple "1,3" (Just $ piece Light Tall Square Solid)
  ]
