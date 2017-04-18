module GameGraphics (
    PivotPosition(..),
    PaperItem,
    PaperProject,
    GRAPHICS,
    getPaperProject,
    getPaperItem,
    AffGraphics,
    addMiniPiecesTo,
    hideMessage,
    showMessage,
    hidePaperItem,
    movePaperItem,
    newGame,
    showPaperItem,
    movePaperItemPivot,
    hideGivePieceText,
    hidePlayPieceText,
    showGivePieceText,
    showPlayPieceText,
    loadAssets,
    itemName,
    listenToAvailablePieces,
    listenToBoard,
    createMainMenu,
    drawAvailablePieces,
    drawAvailablePiecesLayout,
    drawBoard,
    init,
    pieceAssets
  ) where

import Prelude (
    class Show, Unit, show, (<>), map
  )
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (Error)
import DOM.Event.Event (Event)
import DOM.HTML.Types (HTMLCanvasElement)
import Data.Foldable (traverse_)
import Data.Function.Uncurried (Fn0, Fn1, Fn2, Fn3, Fn4, mkFn1, mkFn2, runFn0, runFn1, runFn3, runFn4)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap)
import Graphics.Canvas (CanvasImageSource)
import State (Piece, makePieces, miniPieceId, pieceImage)

-- | Effect for graphics using PaperJS
type AffGraphics e a = Aff (graphics :: GRAPHICS | e) a

-- | Possible pivot positions in PaperJS
data PivotPosition
  = TopLeft
  | TopRight
  | BottomLeft
  | BottomRight
  | LeftCenter
  | TopCenter
  | RightCenter
  | BottomCenter

instance showPivotPosition :: Show PivotPosition where
  show TopLeft = "topLeft"
  show TopRight = "topRight"
  show BottomLeft = "bottomLeft"
  show BottomRight = "bottomRight"
  show LeftCenter = "leftCenter"
  show TopCenter = "topCenter"
  show RightCenter = "rightCenter"
  show BottomCenter = "bottomCenter"

foreign import data PaperItem :: Type
foreign import data PaperProject :: Type
foreign import data GRAPHICS :: Effect

foreign import itemName_ :: Fn1 PaperItem String

-- | Given a PaperJS item, get it's name
itemName :: PaperItem -> String
itemName = runFn1 itemName_

foreign import getPaperProject :: HTMLCanvasElement -> PaperProject

foreign import getPaperItem_ :: forall a. Fn4 PaperProject String (a -> Maybe a) (Maybe a) (Maybe PaperItem)

-- | Given a PaperJS project (there can be multiple on the page)
-- | and an item name, attempt to get the item
getPaperItem :: PaperProject -> String -> (Maybe PaperItem)
getPaperItem project name = runFn4 getPaperItem_ project  name Just Nothing

foreign import showPaperItem :: forall e. PaperItem -> AffGraphics e Unit
foreign import hidePaperItem :: forall e. PaperItem -> AffGraphics e Unit

foreign import movePaperItem_ :: forall e. Fn3 Int Int PaperItem (AffGraphics e Unit)

-- | Move a paper item to point x y using the defalt
-- | center pivot point
movePaperItem :: forall e. Int -> Int -> PaperItem -> (AffGraphics e Unit)
movePaperItem = runFn3 movePaperItem_

foreign import movePaperItemPivot_ :: forall e. Fn4 String Int Int PaperItem (AffGraphics e Unit)

-- | Move a paper item to point x y, but position it using
-- | a specific pivot point.
movePaperItemPivot :: forall e. PivotPosition -> Int -> Int -> PaperItem -> (AffGraphics e Unit)
movePaperItemPivot pp x y pi = runFn4 movePaperItemPivot_ (show pp) x y pi

-- | Initialize the paper scope
foreign import init :: forall e. AffGraphics e Unit

foreign import listenToBoard_ :: forall e a.
  Fn1
  (Fn2 PaperItem Event (AffGraphics e a))
  (AffGraphics e Unit)

-- | Listen for events on the game board
listenToBoard :: forall e a. (PaperItem -> Event -> AffGraphics e a) -> (AffGraphics e Unit)
listenToBoard fn = runFn1 listenToBoard_ (mkFn2 fn)

foreign import drawBoard_ :: forall e. Fn0 (AffGraphics e Unit)

-- | Draw the game board
drawBoard :: forall e. AffGraphics e Unit
drawBoard = runFn0 drawBoard_

-- | Graphically reset the game board, removing all pieces
-- | and returning them to the side.
foreign import newGame :: forall e. AffGraphics e Unit

showGivePieceText :: forall e. AffGraphics e Unit
showGivePieceText = showMessage "Choose a piece to give your opponent."

showPlayPieceText :: forall e. AffGraphics e Unit
showPlayPieceText = showMessage "Play your piece."

hideGivePieceText :: forall e. AffGraphics e Unit
hideGivePieceText = hideMessage

hidePlayPieceText :: forall e. AffGraphics e Unit
hidePlayPieceText = hideMessage

-- | Show a message to the user above the play area
foreign import showMessage :: forall e. String -> AffGraphics e Unit

-- | Hide a message to the user above the play area
foreign import hideMessage :: forall e. AffGraphics e Unit

foreign import createMainMenu_ :: forall e. (Fn1 String (AffGraphics e Unit)) -> AffGraphics e Unit

createMainMenu :: forall e. (String -> AffGraphics e Unit) -> AffGraphics e Unit
createMainMenu fn = runFn1 createMainMenu_ (mkFn1 fn)

foreign import loadAssets_ :: forall e. Fn4
  (Array String)
  (Array String)
  ((StrMap CanvasImageSource) -> Eff (graphics :: GRAPHICS | e) Unit)
  (Error -> Eff (graphics :: GRAPHICS | e) Unit)
  (Eff (graphics :: GRAPHICS | e) Unit)

loadAssets :: forall e. Array String -> Array String -> (Aff (graphics :: GRAPHICS | e) (StrMap CanvasImageSource))
loadAssets src ids = makeAff (\err s -> (runFn4 loadAssets_) src ids s err)

foreign import listenToAvailablePieces_ :: forall e a.
  Fn1
  (Fn2 PaperItem Event (AffGraphics e a))
  (AffGraphics e Unit)

listenToAvailablePieces :: forall e. (PaperItem -> Event -> (AffGraphics e Unit)) -> (AffGraphics e Unit)
listenToAvailablePieces fn = runFn1 listenToAvailablePieces_ (mkFn2 fn)

foreign import drawAvailablePieces_ :: forall e. Fn0 (AffGraphics e Unit)

drawAvailablePieces :: forall e. (AffGraphics e Unit)
drawAvailablePieces = runFn0 drawAvailablePieces_

foreign import drawAvailablePiecesLayout :: forall e. AffGraphics e Unit

prependAssetPath :: String -> String
prependAssetPath s = "images/" <> s

pieceAssets :: Array String
pieceAssets = (map prependAssetPath (map pieceImage makePieces))

showMini :: forall e. PaperProject -> Piece -> AffGraphics e Unit
showMini project p = traverse_ showPaperItem (getPaperItem project (miniPieceId p))

hideMini :: forall e. PaperProject -> Piece -> AffGraphics e Unit
hideMini project p = traverse_ hidePaperItem (getPaperItem project (miniPieceId p))

moveMini :: forall e. PaperProject -> Int -> Int -> Piece -> AffGraphics e Unit
moveMini project x y p = traverse_ (movePaperItem x y) (getPaperItem project (miniPieceId p))

foreign import addMiniPiecesTo :: forall e. PaperProject -> AffGraphics e Unit
