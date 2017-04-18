module AI where

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (all, filterA, foldM, foldMap, fromFoldable, head)
import Data.Maybe (Maybe(Nothing, Just), isJust)
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (unwrap)
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.StrMap (empty)
import Prelude (class Eq, eq, class Ord, class Bounded, compare, top, bottom, (>>>), (>), ($), otherwise, map, pure, bind, (<>), (>>=), (#), (==), (>=), (<<<))
import State (Board, Piece, Point, PositionID, countEmptySpaces, countTriples, emptyBoard, foldMapEmptySpaces, mkPoint, placePieceS', pointId, unplayedPieces, unplayedPoints, winningBoard)
import Util (randomElement)


-- | A data type representing a game play.
-- | A user playing a piece at a point and
-- | the resulting game board along with a score
-- | for how good the board is .
data QuartoPlay = QuartoPlay Piece Point Board Int

instance eqQuartoPlay :: Eq QuartoPlay where
  eq (QuartoPlay _ _ _ score1) (QuartoPlay _ _ _ score2) = eq score1 score2

instance ordQuartoPlay :: Ord QuartoPlay where
  compare (QuartoPlay _ _ _ score1) (QuartoPlay _ _ _ score2) = compare score1 score2

-- Bounded instance for a play, so we can order plays
-- with Min and Max.  Be careful to never let these
-- get into a set of plays.
instance boundedQuartoPlay :: Bounded QuartoPlay where
  bottom = (QuartoPlay empty (mkPoint 1 1) emptyBoard bottom)
  top = (QuartoPlay empty (mkPoint 1 1) emptyBoard top)

getBoard :: QuartoPlay -> Board
getBoard (QuartoPlay _ _ a _) = a

getPoint' :: QuartoPlay -> PositionID
getPoint' = getPoint >>> pointId

getPoint :: QuartoPlay -> Point
getPoint (QuartoPlay _ a _ _) = a

getScore :: QuartoPlay -> Int
getScore (QuartoPlay _ _ _ a) = a

getPiece :: QuartoPlay -> Piece
getPiece (QuartoPlay a _ _ _) = a

-- | Get all possible plays give a board and a piece to play.
aiGetPlays :: forall e. Board -> Piece -> Eff (random :: RANDOM | e) (Array QuartoPlay)
aiGetPlays board piece
  | countEmptySpaces board > 12 = randomPlay' board piece
  | otherwise = pure $
      foldMapEmptySpaces (\point -> do
        let newBoard = (placePieceS' board piece point)
        let score = scoreBoard newBoard board
        [QuartoPlay piece point newBoard score]
      ) board

-- | Choose the best play in a set of plays based on the
-- | score of the plays
bestPlayInSet :: forall e. Array QuartoPlay -> Eff (console :: CONSOLE | e) (Maybe QuartoPlay)
bestPlayInSet plays =
  map unwrap $ foldM (\max' play -> do
    pure $ (max' <> Max (Just play))
  ) (Max Nothing) plays

-- | Choose the worst play in a set of plays based on the
-- | score of the plays
worstPlayInSet :: forall e. Array QuartoPlay -> Eff (console :: CONSOLE | e) (Maybe QuartoPlay)
worstPlayInSet plays =
  map unwrap $ foldM (\min' play -> case min' of
    (Min Nothing) -> pure $ Min (Just play)
    _             -> pure $ min' <> Min (Just play)
  ) (Min Nothing) plays

-- | Look ahead and see what the best give option is after a play had been made.
-- | If the give results in a win for the other player, then filter out
-- | that play.
filterDeadEndPlays :: forall e. Array QuartoPlay -> Eff (console :: CONSOLE, random :: RANDOM | e) (Array QuartoPlay)
filterDeadEndPlays =
  filterA (\play -> do
    give <- bestGive (getBoard play)
    pure $ case give of
      Just _ -> true
      Nothing -> false
  )

-- | Determine what the best play is given a Board and Piece.
bestPlay :: forall e. Board -> Piece -> Eff (console :: CONSOLE, random :: RANDOM | e) (Maybe QuartoPlay)
bestPlay board piece = do
  plays <- aiGetPlays board piece

  let winningPlays = (choosePlay isWinner plays)
  case head winningPlays of
    Nothing -> do
      -- log $ "before: " <> (show $ map (pieceId <<< getPiece) plays)
      noDeadEnds <- filterDeadEndPlays plays
      -- log $ "after: " <> (show $ map (pieceId <<< getPiece) noDeadEnds)
      best <- bestPlayInSet noDeadEnds
      random <- randomPlay board piece
      let choice = best <|> random
      pure choice
    winner -> pure winner

-- | Determine the best piece to give give a Board
bestGive :: forall e. Board -> Eff (console :: CONSOLE, random :: RANDOM | e) (Maybe Piece)
bestGive board = do
  let pieces = unplayedPieces board
  noWinningPlays <- foldM (\ok piece -> do
    plays <- aiGetPlays board piece

    if (all (\play -> (unwrap isNotWinner) (getBoard play)) plays)
      then do
        worstPlayInSet plays >>= case _ of
          (Just worst) -> pure $ ok <> [worst]
          Nothing -> pure ok
      else do
        pure ok

  ) [] pieces

  worst <- worstPlayInSet noWinningPlays

  -- log $ "choices: " <> (show $ map (pieceId <<< getPiece) noWinningPlays)
  -- log $ "final: " <> (show $ map (pieceId <<< getPiece) worst)
  -- log $ "score: " <> (show $ map (getScore) worst)

  case (worst) of
    Just worstPlay -> pure $ Just $ getPiece worstPlay
    _ -> pure Nothing --randomElement pieces

-- | Score a board in comparison to the old board
scoreBoard :: Board -> Board -> Int
scoreBoard newBoard oldBoard = unwrap $ foldMap Additive [
    score_ 200 (newBoard # (unwrap isWinner)),
    score_ 5   (newBoard # (unwrap $ hasTriples 1)),
    score_ 11  (newBoard # (unwrap $ hasTriples 2)),
    score_ 16  (newBoard # (unwrap $ hasTriples 3)),
    score_ 50  (newBoard # (unwrap $ hasMoreTriplesThan oldBoard))
  ]
  where
    score_ val condition =
      if condition
        then val
        else 0


-- | Choose a random play given a board and a pice and return it in an array
randomPlay' :: forall e. Board -> Piece -> Eff (random :: RANDOM | e) (Array QuartoPlay)
randomPlay' board piece = (randomPlay board piece) >>= fromFoldable >>> pure


-- | Choose a random play given a board and a piece and return it as a maybe
randomPlay :: forall e. Board -> Piece -> Eff (random :: RANDOM | e) (Maybe QuartoPlay)
randomPlay board piece = do
  randomElement (unplayedPoints board) >>= case _ of
    Just point -> do
      let newBoard = (placePieceS' board piece point)
      let score = scoreBoard newBoard board
      pure $ Just $ QuartoPlay piece point newBoard score
    Nothing -> pure Nothing

-- | Determine if the board is a winner
isWinner :: Conj (Board -> Boolean)
isWinner = Conj $ (\board -> isJust $ winningBoard board)

-- | Determine if the board is not a winner
isNotWinner :: Conj (Board -> Boolean)
isNotWinner = Conj $ (\board -> case winningBoard board of
    Nothing -> true
    _ -> false
  )

-- | Determine if the board a certain number of triples
hasTriples :: Int -> Conj (Board -> Boolean)
hasTriples count = Conj $ (\board -> (countTriples board) == count)

-- | Determine if the board has a minimum number of triples
hasAtLeastXTriples ::  Int -> Conj (Board -> Boolean)
hasAtLeastXTriples count = Conj $ (\board -> (countTriples board) >= count)

-- | Determine if one board has more triples than another
hasMoreTriplesThan :: Board -> Conj (Board -> Boolean)
hasMoreTriplesThan comparisonBoard = Conj $ (\board -> (countTriples board) > (countTriples comparisonBoard))

-- | Randomly choose a play for every board in the array
chooseRandomPlay :: forall e. Array Board -> Eff (random :: RANDOM | e) (Array Board)
chooseRandomPlay boards = randomElement boards >>= fromFoldable >>> pure

-- | Given a condition and a set of plays, filter out the plays where the condition holds
choosePlay :: Conj (Board -> Boolean) -> Array QuartoPlay -> Array QuartoPlay
choosePlay condition plays = unwrap $ filterA (Conj <<< (unwrap condition) <<< getBoard) plays
