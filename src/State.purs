module State where

import Prelude
import Data.List as L
import Data.Array (filter, length, nub, sortBy)
import Data.Either (Either(..), fromRight)
import Data.Generic (class Generic, gShow)
import Data.Map (Map, empty, fromFoldable, insert, keys, lookup, member, toUnfoldable, values)
import Data.Maybe (Maybe(..))
import Data.Traversable (all, find, foldr, sequence, foldMap)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

data Property
  = Hollow
  | Solid
  | Tall
  | Short
  | Black
  | White
  | Square
  | Circle

derive instance eqProperty :: Eq Property
derive instance ordProperty :: Ord Property
derive instance gProperty  :: Generic Property
instance showProperty :: Show Property where show = gShow

type Piece = Map Property Property
type X = Int
type Y = Int
type Point = Tuple Int Int
type Board = Map Point (Maybe Piece)


propertyClass :: Property -> String
propertyClass Hollow = "h"
propertyClass Solid  = "f" -- full
propertyClass Tall   = "t"
propertyClass Short  = "sh"
propertyClass Black  = "b"
propertyClass White  = "w"
propertyClass Square = "s"
propertyClass Circle = "c"

pieceClasses :: Piece -> String
pieceClasses piece' = foldr (\p a -> (a <> " " <> propertyClass p) ) "" (keys piece')

showPoint :: Point -> String
showPoint (Tuple a b) = (show a) <> ", " <> (show b)

piece :: Property -> Property -> Property -> Property -> Piece
piece p1 p2 p3 p4 = unsafePartial $ fromRight $ (safePiece p1 p2 p3 p4)


safePiece :: Property -> Property -> Property -> Property -> Either String Piece
safePiece p1 p2 p3 p4 = do
  let nubed = nub [p1, p2, p3, p4]
  when (length nubed /= 4) (Left ((show [p1, p2, p3, p4]) <> " is not a valid piece. (duplicate property)"))

  let mpiece = find (\piece' -> true == (all (areMembersOf piece') nubed)) makePieces
  case mpiece of
    (Just a) -> Right a
    (Nothing) -> Left ((show nubed) <> " is not a valid piece. (unknown combination)")

  where areMembersOf m = (flip member) m

makePieces :: Array Piece
makePieces = do
  hs <- [Hollow, Solid]
  ts <- [Tall, Short]
  c  <- [Black, White]
  s  <- [Square, Circle]
  pure $ fromFoldable [
    Tuple hs hs,
    Tuple ts ts,
    Tuple c c,
    Tuple s s
  ]

pieceIs :: Property -> Piece -> Boolean
pieceIs = member

makePoints :: Array Point
makePoints = do
  x <- [1,2,3,4]
  y <- [1,2,3,4]
  pure (Tuple x y)

emptyBoard :: Board
emptyBoard = foldr (\p b -> insert p Nothing b) empty makePoints

winningBoard :: Board -> Array (Maybe Piece)-- (Point)--(Maybe Piece)--(Array Piece)
winningBoard board =
  foldMap (\positions -> do
    --positions
    -- [lookup (Tuple 1 1) board]
    let mpieces = sequence $ map ((flip lookup) board) positions

    case mpieces of
      (Just pieces) -> do
        prop <- [Hollow, Solid, Tall, Short, Black, White, Square, Circle]
        if (all <$> (Just (pieceIs prop)) <*> (sequence pieces)) == (Just true)
          then pieces
          else []
      Nothing -> []
  ) winners
  where
    diagonalWinners = [ [Tuple 1 1, Tuple 2 2, Tuple 3 3, Tuple 4 4]
                      , [Tuple 4 1, Tuple 3 2, Tuple 2 3, Tuple 1 4]]
    straightWinners = do
      v <- [1,2,3,4]
      [ [Tuple 1 v, Tuple 2 v, Tuple 3 v, Tuple 4 v]
      , [Tuple v 1, Tuple v 2, Tuple v 3, Tuple v 4] ]

    winners = (diagonalWinners <> straightWinners)


positionPlayed :: Board -> Point -> Maybe Piece
positionPlayed b p =
  case (lookup p b) of
    (Just mpiece) -> mpiece
    Nothing -> Nothing

piecePlayed :: Board -> Piece -> Boolean
piecePlayed b p = L.elem (Just p) (values b)

unplayedPieces :: Board -> Array Piece
unplayedPieces board = filter (not (piecePlayed board)) makePieces

placePiece :: Board -> Piece -> Point -> Either String Board
placePiece board piece' point = do
  if (positionPlayed board point /= Nothing)
    then Left ((show point) <> " already has a piece there.")
    else
      if (piecePlayed board piece' == true)
        then Left ((show piece') <> " has already been played.")
        else Right $ insert point (Just piece') board

orderPoints :: Tuple Int Int -> Tuple Int Int -> Ordering
orderPoints (Tuple x1 y1) (Tuple x2 y2 )
  | y1 == y2 && x2 > x1  = LT
  | y1 < y2              = LT
  | y1 == y2 && x1 == x2 = EQ
  | otherwise            = GT

unfoldBoard :: Board -> Array (Tuple Point (Maybe Piece))
unfoldBoard b = sortBy (\(Tuple s1 _) (Tuple s2 _) ->
  orderPoints s1 s2
) (toUnfoldable b)
