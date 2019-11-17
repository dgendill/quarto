module GameGraphics (
    PivotPosition(..),
    PaperItem,
    PaperProject,
    getPaperProject,
    getPaperItem,
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
    pieceAssets,
    init
  ) where

import Prelude

import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, mkFn1, mkFn2, runFn1, runFn2, runFn3, runFn4)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Console (logShow)
import Foreign.Object (Object)
import Graphics.Canvas (CanvasImageSource)
import State (makePieces, pieceImage)
import Web.Event.Event (Event)
import Web.HTML.HTMLCanvasElement (HTMLCanvasElement)

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


-- | Initialize the paper scope
foreign import init_ :: EffectFnAff Unit
init :: Aff Unit
init = fromEffectFnAff init_


-- | Given a PaperJS item, get it's name
itemName :: PaperItem -> String
itemName = runFn1 itemName_
foreign import itemName_ :: Fn1 PaperItem String

foreign import getPaperProject :: HTMLCanvasElement -> PaperProject


-- | Given a PaperJS project (there can be multiple on the page)
-- | and an item name, attempt to get the item
getPaperItem :: PaperProject -> String -> (Maybe PaperItem)
getPaperItem project name = runFn4 getPaperItem_ project  name Just Nothing
foreign import getPaperItem_ :: forall a. Fn4 PaperProject String (a -> Maybe a) (Maybe a) (Maybe PaperItem)

foreign import showPaperItem :: PaperItem -> EffectFnAff Unit
foreign import hidePaperItem :: PaperItem -> EffectFnAff Unit

foreign import movePaperItem_ :: Fn3 Int Int PaperItem (EffectFnAff Unit)

-- | Move a paper item to point x y using the defalt
-- | center pivot point
movePaperItem :: Int -> Int -> PaperItem -> (Aff Unit)
movePaperItem a b c = fromEffectFnAff $ runFn3 movePaperItem_ a b c

foreign import movePaperItemPivot_ :: forall e. Fn4 String Int Int PaperItem (EffectFnAff Unit)

-- | Move a paper item to point x y, but position it using
-- | a specific pivot point.
movePaperItemPivot :: PivotPosition -> Int -> Int -> PaperItem -> (Aff Unit)
movePaperItemPivot pp x y pi = fromEffectFnAff $ runFn4 movePaperItemPivot_ (show pp) x y pi


foreign import listenToBoard_ :: forall a.
  Fn2
  (Fn2 PaperItem Event (Aff a))
  ForeignAffHandler
  (EffectFnAff Unit)

-- | Listen for events on the game board
listenToBoard :: forall a. (PaperItem -> Event -> Aff a) -> (Aff Unit)
listenToBoard fn = fromEffectFnAff $ runFn2 listenToBoard_ (mkFn2 fn) foreignAffHandler

foreign import drawBoard_ :: EffectFnAff Unit

-- | Draw the game board
drawBoard :: Aff Unit
drawBoard = fromEffectFnAff drawBoard_

-- | Graphically reset the game board, removing all pieces
-- | and returning them to the side.
foreign import newGame_ :: EffectFnAff Unit
newGame :: Aff Unit
newGame = fromEffectFnAff newGame_

showGivePieceText :: Aff Unit
showGivePieceText = showMessage "Choose a piece to give your opponent."

showPlayPieceText :: Aff Unit
showPlayPieceText = showMessage "Play your piece."

hideGivePieceText :: Aff Unit
hideGivePieceText = hideMessage

hidePlayPieceText :: Aff Unit
hidePlayPieceText = hideMessage

-- | Show a message to the user above the play area
foreign import showMessage_ :: String -> EffectFnAff Unit
showMessage :: String -> Aff Unit
showMessage = fromEffectFnAff <<< showMessage_

-- | Hide a message to the user above the play area
foreign import hideMessage_ :: EffectFnAff Unit

hideMessage :: Aff Unit
hideMessage = fromEffectFnAff hideMessage_

type ForeignAffHandler = Fn1 (Aff Unit) (Effect Unit)

foreignAffHandler :: ForeignAffHandler
foreignAffHandler = (mkFn1 (runAff_ logShow))

foreign import createMainMenu_ :: Fn2 (Fn1 String (Aff Unit)) ForeignAffHandler (EffectFnAff Unit)

createMainMenu :: (String -> Aff Unit) -> Aff Unit
createMainMenu fn = fromEffectFnAff $ (runFn2 createMainMenu_) (mkFn1 fn) foreignAffHandler


foreign import loadAssets_ :: Fn2 (Array String) (Array String) (EffectFnAff (Object CanvasImageSource))

loadAssets :: Array String -> Array String -> Aff (Object CanvasImageSource)
loadAssets src ids = fromEffectFnAff $ (runFn2 loadAssets_) src ids

-- makeAff (\e -> case e of
--     Left e -> pure unit
--     Right s -> (runFn4 loadAssets_) src ids s)

foreign import listenToAvailablePieces_ :: forall a.
  Fn2
  (Fn2 PaperItem Event (Aff a))
  ForeignAffHandler
  (EffectFnAff Unit)

listenToAvailablePieces :: (PaperItem -> Event -> (Aff Unit)) -> (Aff Unit)
listenToAvailablePieces fn = fromEffectFnAff $ runFn2 listenToAvailablePieces_ (mkFn2 fn) foreignAffHandler

foreign import drawAvailablePieces_ :: EffectFnAff Unit

drawAvailablePieces :: Aff Unit
drawAvailablePieces = fromEffectFnAff drawAvailablePieces_

foreign import drawAvailablePiecesLayout_ :: EffectFnAff Unit
drawAvailablePiecesLayout :: Aff Unit
drawAvailablePiecesLayout = fromEffectFnAff drawAvailablePiecesLayout_


prependAssetPath :: String -> String
prependAssetPath s = "images/" <> s

pieceAssets :: Array String
pieceAssets = (map prependAssetPath (map pieceImage makePieces))

foreign import addMiniPiecesTo_ :: PaperProject -> EffectFnAff Unit
addMiniPiecesTo :: PaperProject -> Aff Unit
addMiniPiecesTo = fromEffectFnAff <<< addMiniPiecesTo_
