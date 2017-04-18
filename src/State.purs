module State where

import Prelude
import Data.Array as Array
import Data.Generic.Rep as GR
import Data.List as L
import Control.Apply (lift3, lift4)
import Control.Monad.Except (runExcept, throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Array (concat, filter, foldl, head, length, nub, reverse, snoc, sort, tail)
import Data.Either (Either(..), either, fromRight)
import Data.Foreign (F, Foreign, ForeignError(ForeignError), readInt, readString)
import Data.Foreign.JSON (parseJSON)
import Data.Function.Memoize (class Tabulate, memoize)
import Data.Generic (class Generic, gShow)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lazy (defer)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isNothing)
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (StrMap, empty, fromFoldable, insert, lookup, member, values)
import Data.String (Pattern(..), Replacement(..), joinWith, replace, split)
import Data.Traversable (all, find, foldr, sequence, foldMap)
import Data.Tuple (Tuple(Tuple))
import Partial.Unsafe (unsafePartial)
import Util (spanN)


data Property
  = Hollow
  | Solid
  | Tall
  | Short
  | Dark
  | Light
  | Square
  | Circle

type PieceID = String
type PositionID = String
type Piece = StrMap Property
type X = Int
type Y = Int
type Board = StrMap (Maybe Piece)

newtype Point = Point { x :: Int, y :: Int }
newtype GPiece = GPiece Piece
newtype GBoard = GBoard Board

derive instance gProperty  :: Generic Property
derive instance eqProperty :: Eq Property
derive instance newtypePiece :: Newtype GPiece _
derive instance gPiece :: GR.Generic GPiece _
derive instance gRPoint :: GR.Generic Point _
derive instance gBoard :: GR.Generic GBoard _

instance showProperty :: Show Property where show = gShow
instance encodeProperty :: EncodeJson Property where encodeJson = gEncodeJson
instance decodeProperty :: DecodeJson Property where decodeJson = gDecodeJson
instance semigroupProperty :: Semigroup Property where
  append p1 p2 = p1

instance tabulatePiece :: Tabulate GPiece where
  tabulate f piece' = defer (\_ -> f piece')

instance eqRPoint :: Eq Point where eq = genericEq
instance showRPoint :: Show Point where show = genericShow
instance ordRPoint :: Ord Point where
  compare (Point { x : x1, y : y1}) (Point { x : x2, y : y2}) = compare (Tuple x1 y1) (Tuple x2 y2)

instance tabulateBoard :: Tabulate GBoard where
  tabulate f board = defer (\_ -> f board)

-- Light/Dark < Tall/Short < Circle/Square < Solid/Hollow
instance ordProperty :: Ord Property where
  compare Dark Light = LT
  compare Light Dark = GT
  compare Short Tall = LT
  compare Tall Short = GT
  compare Square Circle = LT
  compare Circle Square = GT
  compare Hollow Solid = LT
  compare Solid Hollow = GT
  compare p1 p2
    | p1 == p2 = EQ
    | pIsColor p1 = LT
    | pIsColor p2 = GT
    | pIsHeight p1 = LT
    | pIsHeight p2 = GT
    | pIsShape p1 = LT
    | pIsShape p2 = GT
    | pIsFilling p1 = LT
    | pIsFilling p2 = GT
    | otherwise = EQ


-- | The Foreign representation of a point is
-- | number,number
readPoint :: Foreign -> F Point
readPoint value = do
  s <- readString value
  case positionIdToPoint s of
    Right point -> pure point
    Left error -> throwError $ singleton (ForeignError error)


both :: (Property -> Boolean) -> Property -> Property -> Boolean
both f p1 p2 = (f p1) && (f p2)


beither :: (Property -> Boolean) -> Property -> Property -> Boolean
beither f p1 p2 = (f p1) || (f p2)


pIsColor :: Property -> Boolean
pIsColor p = if p == Dark || p == Light then true else false


pIsShape :: Property -> Boolean
pIsShape p = if p == Square || p == Circle then true else false


pIsHeight :: Property -> Boolean
pIsHeight p = if p == Short || p == Tall then true else false


pIsFilling :: Property -> Boolean
pIsFilling p = if p == Hollow || p == Solid then true else false


-- | Show a nice visual representation of the board
showBoard :: Board -> String
showBoard board = showRows $ spanN 4 positions
  where
  positions = foldMap (\position ->
    case (lookup (pointId position) board) of
      (Just mpiece) -> [Tuple position mpiece]
      Nothing -> [Tuple position Nothing]
    ) orderedPoints

  showRows :: { init :: Array (Tuple Point (Maybe Piece)), rest :: Array (Tuple Point (Maybe Piece)) } -> String
  showRows { init : init, rest : rest} =
    let s = foldMap (\(Tuple point piece') -> do
              case piece' of
                (Just p) -> " (" <> (simpleShowPiece p) <> ") "
                Nothing ->  " ______________ "
            ) init
    in
      case (rest) of
        [] -> s
        _  -> s <> "\n\n" <> (showRows $ spanN 4 rest)


-- | Helper function to make a Point
mkPoint :: Int -> Int -> Point
mkPoint x y = Point { x : x, y : y}


-- | Attempt to parse a Point from a string
positionIdToPoint :: String -> Either String Point
positionIdToPoint s = fromMaybe (Left "Could not parse position structure.") $
  (either (const $ Left "Values in position were not integers.") (\m -> Right m)) <$> do
    let xy = map (\v -> runExcept $ parseJSON v >>= readInt) (split (Pattern ",") s)
    x <- head xy
    t <- tail xy
    y <- head t
    pure $ mkPoint <$> x <*> y


-- | Convert a Piece to a PieceID (String with format "number,number")
pieceId :: Piece -> PieceID
pieceId piece' = "id-" <> propsString
  where
    propsArray = foldMap (\p -> [propertyName p] ) (sort $ Array.fromFoldable (values piece'))
    propsString = joinWith "-" propsArray


-- | Get the paperjs id for a the mini pieces used in the
-- | win menu.
miniPieceId :: Piece -> String
miniPieceId p = append (pieceId p) "-mini"


-- | Unsafely get a piece from a string
unsafePieceIdToPiece :: String -> Piece
unsafePieceIdToPiece = unsafePartial $ fromRight <<< pieceIdToPiece


-- | Attempt to get a Piece from a PieceID (string with format id-prop-prop-prop-prop
pieceIdToPiece :: String -> Either String Piece
pieceIdToPiece s = fromMaybe (Left "The piece could not be parsed from the id.") $ do
  let parts = map propertyNameToProperty (split (Pattern "-") (replace (Pattern "id-") (Replacement "") s))
  p1 <- head parts
  t1 <- tail parts
  p2 <- head t1
  t2 <- tail t1
  p3 <- head t2
  t3 <- tail t2
  p4 <- head t3
  lift4 safePiece p1 p2 p3 p4


-- | Get the image asset for the piece
pieceImage :: Piece -> String
pieceImage piece' = "piece (" <> propsString <> ").png"
  where
    propsArray = foldMap (\p -> [propertyName p] ) (sort $ Array.fromFoldable (values piece'))
    propsString = joinWith " " propsArray
-- "images/piece (Light Tall Square Solid).png",
-- "images/piece (Light Short Circle Hollow).png",
-- "images/piece (Dark Short Circle Hollow).png",
-- "images/piece (Dark Short Circle Solid).png",
-- "images/piece (Light Short Circle Solid).png",
-- "images/piece (Light Short Square Hollow).png",
-- "images/piece (Dark Short Square Hollow).png",
-- "images/piece (Light Short Square Solid).png",
-- "images/piece (Light Tall Circle Hollow).png",
-- "images/piece (Dark Short Square Solid).png",
-- "images/piece (Dark Tall Circle Hollow).png",
-- "images/piece (Light Tall Circle Solid).png",
-- "images/piece (Dark Tall Circle Solid).png",
-- "images/piece (Light Tall Square Hollow).png",
-- "images/piece (Dark Tall Square Hollow).png",
-- "images/piece (Light Tall Square Solid).png"


-- | Short hand indicators for a property type
propertyClass :: Property -> String
propertyClass Hollow = "h "
propertyClass Solid  = "f "
propertyClass Tall   = "t "
propertyClass Short  = "sh"
propertyClass Dark  = "b "
propertyClass Light  = "w "
propertyClass Square = "s "
propertyClass Circle = "c "


-- | Show a short representation of a piece
simpleShowPiece :: Piece -> String
simpleShowPiece = pieceClasses


-- | Get the Short hand indicators for a property type
pieceClasses :: Piece -> String
pieceClasses piece' = foldr (\p a -> (a <> " " <> propertyClass p) ) "" (values piece')


-- | Visually display a property type
propertyName :: Property -> String
propertyName Hollow = "Hollow"
propertyName Solid  = "Solid" -- full
propertyName Tall   = "Tall"
propertyName Short  = "Short"
propertyName Dark  = "Dark"
propertyName Light  = "Light"
propertyName Square = "Square"
propertyName Circle = "Circle"


-- | Get a Property from a String
propertyNameToProperty :: String -> Maybe Property
propertyNameToProperty "Hollow" = Just Hollow
propertyNameToProperty "Solid"  = Just Solid
propertyNameToProperty "Tall"   = Just Tall
propertyNameToProperty "Short"  = Just Short
propertyNameToProperty "Dark"  = Just Dark
propertyNameToProperty "Light"  = Just Light
propertyNameToProperty "Square" = Just Square
propertyNameToProperty "Circle" = Just Circle
propertyNameToProperty _ = Nothing


-- | All possible properties
properties :: Array Property
properties = [Hollow, Solid, Tall, Short, Dark, Light, Square, Circle]


-- | Show a point
showPoint :: Point -> String
showPoint (Point r) = (show r.x) <> "," <> (show r.y)


-- | Get a point's id
pointId :: Point -> String
pointId (Point r) = (show r.x) <> "," <> (show r.y)


-- | Unsafely parse a piece from a set of properties.  This
-- | can result in non-existant, invalid pieces
piece :: Property -> Property -> Property -> Property -> Piece
piece p1 p2 p3 p4 = unsafePartial $ fromRight $ (safePiece p1 p2 p3 p4)


-- | Safely parse a Piece from a set of properties.  Non-existant
-- | pieces aren't possible.
safePiece :: Property -> Property -> Property -> Property -> Either String Piece
safePiece p1 p2 p3 p4 = do
  let nubed = nub (map propertyName [p1, p2, p3, p4])
  when (length nubed /= 4) (Left ((show [p1, p2, p3, p4]) <> " is not a valid piece. (duplicate property)"))
  let mpiece = find (\piece' -> (all (areMembersOf piece') nubed)) makePieces
  case mpiece of
    (Just a) -> Right a
    (Nothing) -> Left ((show nubed) <> " is not a valid piece. (unknown combination)")

  where areMembersOf m = (flip member) m


-- | Get all pieces in the game
makePieces :: Array Piece
makePieces = do
  hs <- [Hollow, Solid]
  ts <- [Tall, Short]
  c  <- [Dark, Light]
  s  <- [Square, Circle]
  pure $ fromFoldable [
    Tuple (propertyName c) c,
    Tuple (propertyName ts) ts,
    Tuple (propertyName s) s,
    Tuple (propertyName hs) hs
  ]


-- | Test if a pieces has a particular property
pieceIs :: Property -> Piece -> Boolean
pieceIs property piece' = member (propertyName property) piece'


-- | Since there is a 3D-sort-of-look to the board,
-- | we need to segregate the points into layer. The points
-- | in the background are traversed before the points in the
-- | forground
visuallyOrderedPoints :: Array Point
visuallyOrderedPoints = reverse $ [
  Point { x : 1, y : 4 },
  Point { x : 1, y : 3 },
  Point { x : 2, y : 4 },
  Point { x : 1, y : 2 },
  Point { x : 2, y : 3 },
  Point { x : 3, y : 4 },
  Point { x : 1, y : 1 },
  Point { x : 2, y : 2 },
  Point { x : 3, y : 3 },
  Point { x : 4, y : 4 },
  Point { x : 2, y : 1 },
  Point { x : 3, y : 2 },
  Point { x : 4, y : 3 },
  Point { x : 3, y : 1 },
  Point { x : 4, y : 2 },
  Point { x : 4, y : 1 }
]


-- | Get all points in a sequential order
orderedPoints :: Array Point
orderedPoints = do
  x <- [1,2,3,4]
  y <- [1,2,3,4]
  pure (Point { x : x, y : y })


-- | Get all points in the game
makePoints :: Array Point
makePoints = orderedPoints


-- | An empty board
emptyBoard :: Board
emptyBoard = foldr (\p b -> insert p Nothing b) empty (map pointId makePoints)


-- | Determine if the board is a Draw (no winner)
isDraw :: Board -> Boolean
isDraw board = (length (unplayedPieces board) == 0) && (isNothing (winningBoard board))


-- | Determine if the board is a winner, and cache the
-- | result for faster computation.
winningBoard :: Board -> Maybe (Array Piece)
-- winningBoard = winningBoardSlow
winningBoard board = (map >>> map) unwrap $ memoize (\(GBoard board') -> (map >>> map) GPiece (winningBoardSlow board')) (GBoard board)


-- | Determine if the board is a winner
winningBoardSlow :: Board -> Maybe (Array Piece)
winningBoardSlow board =
  if length winners > 0
    then sequence winners
    else Nothing
  where
    diagonalWinners = [ [mkPoint 1 1, mkPoint 2 2, mkPoint 3 3, mkPoint 4 4]
                      , [mkPoint 4 1, mkPoint 3 2, mkPoint 2 3, mkPoint 1 4]]
    straightWinners = do
      v <- [1,2,3,4]
      [ [mkPoint 1 v, mkPoint 2 v, mkPoint 3 v, mkPoint 4 v]
      , [mkPoint v 1, mkPoint v 2, mkPoint v 3, mkPoint v 4] ]

    allWinners = ((map >>> map) pointId (diagonalWinners <> straightWinners))

    winners = foldMap (\positions -> do
      let mpieces = sequence $ map ((flip lookup) board) positions

      case mpieces of
        (Just pieces) -> do
          prop <- [Hollow, Solid, Tall, Short, Dark, Light, Square, Circle]
          if (all <$> (Just (pieceIs prop)) <*> (sequence pieces)) == (Just true)
            then pieces
            else []
        Nothing -> []
    ) allWinners


-- | Determine if a point on the board has a piece
-- | on it.
positionPlayed :: Board -> Point -> Maybe Piece
positionPlayed b p =
  case (lookup (pointId p) b) of
    (Just mpiece) -> mpiece
    Nothing -> Nothing


-- | Determine if a particular piece has been
-- | played yet.
piecePlayed :: Board -> Piece -> Boolean
piecePlayed b p = L.elem (Just p) (values b)


-- | Get all unplayed pieces
unplayedPieces :: Board -> Array Piece
unplayedPieces board = filter (not (piecePlayed board)) makePieces


-- | Place a piece on the board at a certain point (using ADTs)
-- | Returns the empty board if something went wrong.
placePieceS' :: Board -> Piece -> Point -> Board
placePieceS' board piece' point = (placePieceS board) (pieceId piece') (pointId point)


-- | Place a piece on the board at a certain point (using IDs).
-- | Returns the empty board if something went wrong.
placePieceS :: Board -> PieceID -> PositionID -> Board
placePieceS board piece' position =
  either (const emptyBoard) (\s ->
    either (const emptyBoard) (\m -> m) s
  ) r
  where
    r = lift3 placePiece (Right board) (pieceIdToPiece piece') (positionIdToPoint position)


-- | Place a piece on the board at a certain point and return errors
-- | if something went wrong.
placePiece :: Board -> Piece -> Point -> Either String Board
placePiece board piece' point = do
  if (positionPlayed board point /= Nothing)
    then Left ((show point) <> " already has a piece there.")
    else
      if (piecePlayed board piece' == true)
        then Left ((show piece') <> " has already been played.")
        else Right $ insert (pointId point) (Just piece') board


-- | Unsafely play a piece
placePieceUnsafe  :: Board -> Piece -> Point -> Board
placePieceUnsafe board piece' point = insert (pointId point) (Just piece') board


-- | Test if a set of pieces all have the same property
allAre :: Array Piece -> Property -> Boolean
allAre pieces property = unwrap $ foldMap (Conj <<< pieceIs property) pieces


-- | Return all the pieces at certain points on the board.  If any of the points
-- | are empty return Nothing.
piecesAtPoints :: Array Point -> StrMap (Maybe Piece) -> Maybe (Array Piece)
piecesAtPoints points board = case (sequence $ (lookup <$> (pointId <$> points)) <*> pure board) of
  (Just m) -> sequence m
  _ -> Nothing


-- | Determine the number of empty spaces on the board
countEmptySpaces :: Board -> Int
countEmptySpaces = foldEmptySpaces (\a _ -> a + 1 ) 0


-- | Get the unplayed points on the board
unplayedPoints :: Board -> Array Point
unplayedPoints = foldEmptySpaces snoc []


-- | Fold over the board's empty spaces
foldEmptySpaces :: forall a. (a -> Point -> a) -> a -> Board -> a
foldEmptySpaces f acc board = foldl (\a point ->
    case lookup (pointId point) board of
      (Just (Just _)) -> a
      _ -> f a point
  ) acc orderedPoints


-- | Fold over the board's empty spaces, accumulating results in
-- | a Monoid.
foldMapEmptySpaces :: forall a. (Monoid a) => (Point -> a) -> Board -> a
foldMapEmptySpaces f board = foldEmptySpaces (\a p -> a <> f p) mempty board


-- | Count the number of "triples" on the board and cache the results for
-- | faster computation
countTriples :: Board -> Int
countTriples board = memoize (\(GBoard board') -> countTriplesSlow board') (GBoard board)


-- | Count the number of "triples" on the board
countTriplesSlow :: Board -> Int
countTriplesSlow board = unwrap $ foldMap id do
  triple <- triples
  let matches = countMatches triple
  pure $ either (const $ Additive 0) (Additive) matches

  where
  countMatches :: Array Point -> Either Int Int
  countMatches points = do
    let mpieces = piecesAtPoints points board

    -- Maybe Boolean
    let threeHaveSameProp =
          fromMaybe false (
            (allAre <$> mpieces) >>=
            (\f -> pure $ unwrap $ foldMap (f >>> Disj) properties)
          )

    case threeHaveSameProp of
      true -> Right 1
      false -> Right 0


-- | A somewhat complete list of all points that
-- | we can define as "triples"
triples :: Array (Array Point)
triples = concat [diag, concat straight]
  where
  diag = [ [mkPoint 1 1, mkPoint 2 2, mkPoint 3 3]
         , [mkPoint 2 2, mkPoint 3 3, mkPoint 4 4]
         , [mkPoint 1 4, mkPoint 2 3, mkPoint 3 2]
         , [mkPoint 4 1, mkPoint 3 2, mkPoint 2 3]
         , [mkPoint 1 1, mkPoint 2 2, mkPoint 4 4]
         , [mkPoint 1 1, mkPoint 3 3, mkPoint 4 4]
         , [mkPoint 1 4, mkPoint 2 3, mkPoint 4 1]
         , [mkPoint 1 4, mkPoint 3 2, mkPoint 4 1]
         ]

  straight = do
    -- Up and down, left and right
    y <- [1,2,3,4]

    -- The first three and last three, and the middle combo
    s <- [[1,2,3],[2,3,4],[1,2,4],[1,3,4]]

    -- horizontally
    let h = foldl (\a b -> snoc a (mkPoint b y)) [] s
    -- vertically
    let v = foldl (\a b -> snoc a (mkPoint y b)) [] s

    pure $ [v, h]
