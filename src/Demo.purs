module Demo (
    slide1,
    slide2,
    slide3,
    slide4,
    slide5,
    runDemo
  ) where

import Animation (animatePieceToDeck, animatePieceToHome, animatePieceToPosition)
import Control.Coroutine (Consumer, Producer, await, pullFrom, runProcess)
import Control.Coroutine.Aff (close, emit, produceAff)
import Control.Monad.Writer (lift)
import Control.Parallel (parSequence, parSequence_)
import Data.Array (zipWithA)
import Data.Foldable (traverse_)
import Data.Function.Uncurried (Fn1, Fn5, Fn6, mkFn1, runFn5, runFn6)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff, forkAff, runAff_)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class.Console (log, logShow)
import Menus (hideGame, showMainMenu)
import Prelude (Unit, bind, map, pure, unit, void, ($), (*>), (>>=), discard)
import Shapes (GraphicItem, labelArrow, removeGraphicItem, removeGraphicItemByName, tooltip)

type ForeignAffHandler = Fn1 (Aff Unit) (Effect Unit)

foreignAffHandler :: ForeignAffHandler
foreignAffHandler = (mkFn1 $ runAff_ logShow)

runDemo :: Aff Unit
runDemo = do
  slide1
  pure unit

slide1 :: Aff Unit
slide1 = do
  let slideGraphic = sequence $ [
      labelArrow { name : "la1", x : 310, y : 97, content : "Tall Hollow", width : 116 }
    , labelArrow { name : "la2", x : 310, y : 215, content : "Short Hollow", width : 116 }
    , labelArrow { name : "la3", x : 310, y : 340, content : "Tall Solid", width : 116 }
    , labelArrow { name : "la4", x : 310, y : 460, content : "Short Solid", width : 116 }
    , tooltip {
        name : "slide1",
        x : 500,
        y : 100,
        width : 185,
        height : 185,
        content : """Each game piece is:

  - Dark or Light
  - Tall or Short
  - Square or Circle
  - Hollow or Solid"""
      }
    ]

  void $ slideGraphic

  let
    cleanup' = cleanup (\message -> do
      case (message) of
        "next" -> do
          removeGraphicItemByName "pb1"
          slideGraphic >>= traverse_ removeGraphicItem
          slide2
        "prev" -> do
          removeGraphicItemByName "nb1"
          slideGraphic >>= traverse_ removeGraphicItem
          showMainMenu *> hideGame
        _      -> do
          log "other"
          pure unit
    )

  void $ forkAff $ runProcess $ pullFrom cleanup' (nextButton' "nb1" 622 260)
  void $ forkAff $ runProcess $ cleanup' `pullFrom` (prevButton' "pb1" 510 260)

  pure unit


cleanup :: (String -> Aff Unit) -> Consumer String Aff Unit
cleanup cb = do
  s <- await
  lift (cb s)


nextButtonNamed' :: String -> Int -> Int -> String -> Producer String Aff Unit
nextButtonNamed' s x y content = produceAff (\emitter -> do
    void $ nextButtonNamed s x y content (do
      emit emitter "next"
      close emitter unit
    )
  )


nextButton' :: String -> Int -> Int -> Producer String Aff Unit
nextButton' s x y = nextButtonNamed' s x y "NEXT"


prevButton' :: String -> Int -> Int -> Producer String Aff Unit
prevButton' s x y = produceAff (\emitter -> do
    void $ prevButton s x y (do
      emit emitter "prev"
    )
  )


slide2 :: Aff Unit
slide2 = do
  parSequence_
    [ animatePieceToPosition "id-Dark-Tall-Square-Hollow" "4,4"
    , animatePieceToPosition "id-Dark-Tall-Circle-Hollow" "4,3"
    , animatePieceToPosition "id-Dark-Short-Square-Hollow" "4,2"
    , animatePieceToPosition "id-Dark-Tall-Square-Solid" "4,1"
    ]

  g <- slideGraphics

  let
    cleanup' = cleanup (\message -> do
      case (message) of
        "next" -> removeGraphicItemByName "pb2"
        "prev" -> removeGraphicItemByName "nb2"
        _      -> pure unit

      traverse_ removeGraphicItem g

      void $ parSequence
        [ animatePieceToHome "id-Dark-Tall-Square-Hollow"
        , animatePieceToHome "id-Dark-Tall-Circle-Hollow"
        , animatePieceToHome "id-Dark-Short-Square-Hollow"
        , animatePieceToHome "id-Dark-Tall-Square-Solid" ]

      case (message) of
        "next" -> slide3
        "prev" -> slide1
        _      -> pure unit
    )

  void $ forkAff $ runProcess $ cleanup' `pullFrom` (prevButton' "pb2" 510 260)
  void $ forkAff $ runProcess $ cleanup' `pullFrom` (nextButton' "nb2" 622 260)

  where
    slideGraphics = sequence $
      [ tooltip {
            name : "slide2",
            x : 500,
            y : 146,
            width : 270,
            height : 140,
            content :  """The goal is to get
four similar pieces in a row. For
instance, four dark pieces in a
row..."""
        }
      ]

slide3 :: Aff Unit
slide3 = do
  let pieces = [
    "id-Dark-Tall-Square-Hollow",
    "id-Light-Tall-Square-Hollow",
    "id-Dark-Short-Square-Hollow",
    "id-Dark-Tall-Square-Solid"
  ]

  g <- sequence $
    [ tooltip {
          name : "slide3",
          x : 500,
          y : 146,
          width : 310,
          height : 140,
          content :  """Or four square pieces in a row.
All pieces in a winning row must
have a matching characteristic to
win."""
      }
    ]

  let
    cleanup' = cleanup (\message -> do
      traverse_ removeGraphicItem g

      case (message) of
        "next" -> removeGraphicItemByName "pb3"
        "prev" -> removeGraphicItemByName "nb3"
        _      -> pure unit

      void $ parSequence $ map animatePieceToHome pieces

      case (message) of
        "next" -> slide4
        "prev" -> slide2
        _      -> pure unit

      pure unit

    )

  void $ forkAff $ runProcess $ cleanup' `pullFrom` (prevButton' "pb3" 510 260)
  void $ forkAff $ runProcess $ cleanup' `pullFrom` (nextButton' "nb3" 622 260)
  void $ zipWithA (\a b -> forkAff $ animatePieceToPosition a b) pieces ["1,4","2,4","3,4","4,4"]

slide4 :: Aff Unit
slide4 = do
  let piece = "id-Dark-Tall-Square-Hollow"
  void $ animatePieceToDeck piece
  g <- sequence $
    [ tooltip {
          name : "slide4",
          x : 500,
          y : 146,
          width : 285,
          height : 140,
          content :  """The game starts by giving your
opponent a piece. Your opponent
then plays the piece, and then
gives you a piece to play."""
      }
    ]

  let
    cleanup' = cleanup (\message -> do
      traverse_ removeGraphicItem g

      case (message) of
        "next" -> removeGraphicItemByName "pb4"
        "prev" -> do
          removeGraphicItemByName "nb4"
          void $ parSequence $ map animatePieceToHome [piece]

        _      -> pure unit

      case (message) of
        "next" -> slide5
        "prev" -> slide3
        _      -> pure unit

      pure unit
    )

  void $ forkAff $ runProcess $ cleanup' `pullFrom` (prevButton' "pb4" 510 260)
  void $ forkAff $ runProcess $ cleanup' `pullFrom` (nextButton' "nb4" 622 260)


slide5 :: Aff Unit
slide5 = do
  let piece = "id-Dark-Tall-Square-Hollow"

  void $ parSequence $ [animatePieceToPosition "id-Dark-Tall-Square-Hollow" "1,4"]

  g <- sequence $
    [ tooltip {
          name : "slide5",
          x : 500,
          y : 146,
          width : 298,
          height : 140,
          content :  """Continue giving and playing pieces
until someone gets four in a row or
all pieces have been played."""
      }
    ]

  let
    cleanup' = cleanup (\message -> do
      traverse_ removeGraphicItem g

      case (message) of
        "next" -> do
          removeGraphicItemByName "pb5"
          void $ parSequence $ [animatePieceToHome "id-Dark-Tall-Square-Hollow"]
        "prev" -> removeGraphicItemByName "nb5"
        _      -> pure unit

      case (message) of
        "next" -> showMainMenu *> hideGame
        "prev" -> slide4
        _      -> pure unit

      pure unit
    )

  void $ forkAff $ runProcess $ cleanup' `pullFrom` (prevButton' "pb5" 510 260)
  void $ forkAff $ runProcess $ cleanup' `pullFrom` (nextButtonNamed' "nb5" 622 260 "MENU")

type ShapeID = String

foreign import nextButton_ ::
  Fn6
  ShapeID
  Int
  Int
  String
  (Aff Unit) ForeignAffHandler
  (EffectFnAff GraphicItem)

foreign import prevButton_ ::
  Fn5
  ShapeID
  Int
  Int
  (Aff Unit) ForeignAffHandler
  (EffectFnAff GraphicItem)

nextButton :: 
     ShapeID
  -> Int
  -> Int
  -> (Aff Unit)
  -> (Aff GraphicItem)
nextButton s x y cb = fromEffectFnAff $ runFn6 nextButton_ s x y "NEXT" cb foreignAffHandler

nextButtonNamed ::
     ShapeID
  -> Int
  -> Int
  -> String
  -> (Aff Unit)
  -> (Aff GraphicItem)
nextButtonNamed id x y name cb = fromEffectFnAff $ runFn6 nextButton_ id x y name cb foreignAffHandler

prevButton ::
     ShapeID
  -> Int
  -> Int
  -> (Aff Unit)
  -> (Aff GraphicItem)
prevButton s x y cb = fromEffectFnAff $ runFn5 prevButton_ s x y cb foreignAffHandler
