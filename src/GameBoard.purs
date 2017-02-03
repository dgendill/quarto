module GameBoard where

import Prelude
import GameSpace as GameSpace
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HHC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import State as S
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Either (fromRight)
import Data.Either (Either(..), either)
import Data.Lazy (defer)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Traversable (foldMap, foldr)
import Data.Tuple (Tuple(..))
import Halogen (ClassName(..))
import Partial.Unsafe (unsafePartial)
import State (Board, Piece, Point, emptyBoard, makePoints, placePiece, positionPlayed, unfoldBoard, unplayedPieces, pieceClasses)

type State =
  { board :: Board
  , ondeck :: Maybe Piece }

data Query a
  = Initialize a
  | Finalize a
  | GivePiece Piece a
  | Play Point a
  | IsPlayed Point (Maybe Piece -> a)

type Input = Unit

data Message
  = Played Piece Point
  | BadPlay String (Maybe Piece) Point


gameBoard :: forall eff. H.Component HH.HTML Query Input Message (Aff (console :: CONSOLE | eff))
gameBoard =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Just (H.action Finalize)
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { board : either
                (const emptyBoard)
                id
                (placePiece emptyBoard (S.piece S.Tall S.White S.Hollow S.Square) (Tuple 4 4))
    , ondeck : Nothing }

  listen :: Point -> GameSpace.Message -> Maybe (Query Unit)
  listen p = Just <<< case _ of
    GameSpace.PlayedPoint point -> H.action (Play point)

  render :: State -> H.ParentHTML Query GameSpace.Query Point (Aff (console :: CONSOLE | eff))
  render state =
    HH.div_
      [ HH.div
        [HP.id_ "available"]

        (foldMap (\piece ->
          [ HH.div
            [HP.classes $ HH.ClassName <$> ["placeholder", (pieceClasses piece)]
            ,HE.onClick (HE.input_ $ GivePiece piece) ]
            []
          ]
        ) (unplayedPieces state.board))

      , HH.div
        [HP.id_ "board-wrapper"]
        [ HH.div
          [HP.id_ "board"]
          (foldMap (\(Tuple point mpiece) ->

              [(HH.slot point (GameSpace.gameSpace point mpiece) mpiece (listen point))]
              -- [(HH.slot point (GameSpace.gameSpace point Nothing) mpiece (HE.input_ $ Play point))]

          ) (unfoldBoard state.board))
        , HH.div
          [HP.id_ "action"]
          [HH.div [HP.class_ $ ClassName "play-message"] [HH.text "Piece to Play"]
          ,HH.div
            [HP.classes $ (HH.ClassName <<< (fromMaybe "")) <$> [Just "placeholder-toplay", pieceClasses >>> ((<>) "placeholder") <$> state.ondeck]
            ]
            []
          ]
        ]
      ]


  eval :: Query ~> H.ParentDSL State Query GameSpace.Query Point Message (Aff (console :: CONSOLE | eff))
  eval = case _ of
    Initialize next -> do
      s <- H.get
      pure next
    Finalize next -> do
      pure next
    Play point next -> do
      liftAff $ log $ "(Board) Playing " <> (show point)
      state <- H.get
      case (state.ondeck) of
        (Just piece) -> do
          liftAff $ log $ "(Board) Playing ondeck piece " <> (show piece)
          let ei = placePiece state.board piece point
          case ei of
            (Right b) -> H.raise $ Played piece point
            (Left e) -> H.raise $ BadPlay e (Just piece) point
        Nothing ->
          H.raise $ BadPlay "There is no piece on deck." Nothing point

      pure next

    GivePiece piece next -> do
      H.modify (\s -> s { ondeck = (Just piece) } )
      pure next

    IsPlayed point reply -> do
      state <- H.get
      pure (reply (positionPlayed state.board point))
