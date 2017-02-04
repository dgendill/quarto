module GameBoard where

import Prelude
import GameSpace as GameSpace
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HHC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import State as S
import State as GState
import Control.Alt (void, (<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (foreachE)
import Control.Monad.State (class MonadState)
import DOM.HTML.HTMLAnchorElement (setDownload)
import Data.Array (foldM, length)
import Data.Either (fromRight)
import Data.Either (Either(..), either)
import Data.Lazy (defer)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid (class Monoid, append, mempty)
import Data.Traversable (foldMap, foldr)
import Data.Tuple (Tuple(..))
import Halogen (ClassName(..))
import Partial.Unsafe (unsafePartial)
import State (Board, Piece, Point, emptyBoard, makePoints, pieceClasses, placePiece, positionPlayed, unfoldBoard, unplayedPieces, winningBoard)

type State =
  { board :: Board
  , ondeck :: Maybe Piece }

data Query a
  = Initialize a
  | Finalize a
  | GivePiece Piece a
  | Play Point a
  | IsPlayed Point (Maybe Piece -> a)
  | Reset a

type Input = Unit

data Message
  = Played Piece Point
  | BadPlay String (Maybe Piece) Point

whenOnDeck :: forall a. (Monoid a) => State -> a -> a
whenOnDeck s yes =
  if (isJust s.ondeck)
  then yes
  else mempty

whenNotOnDeck :: forall a. (Monoid a) => State -> a -> a
whenNotOnDeck s yes =
  if (not $ isJust s.ondeck)
  then yes
  else mempty

whenWinningBoard ::  forall a. (Monoid a) => State -> a -> a
whenWinningBoard s yes =
  if length (winningBoard s.board) > 0
  then yes
  else mempty

maybee :: forall a b. (a -> b) -> b -> (Maybe a) -> b
maybee f d m = fromMaybe d (f <$> m)

gameBoard :: forall eff. Maybe Board -> H.Component HH.HTML Query Input Message (Aff (console :: CONSOLE | eff))
gameBoard iboard =
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
    { board : fromMaybe emptyBoard iboard
    , ondeck : Nothing }

  listen :: Point -> GameSpace.Message -> Maybe (Query Unit)
  listen p = Just <<< case _ of
    GameSpace.PlayedPoint point -> H.action (Play point)


  render :: State -> H.ParentHTML Query GameSpace.Query Point (Aff (console :: CONSOLE | eff))
  render state =
    HH.div_
      [ HH.div
        [HP.id_ "left"]

          (if ((length $ winningBoard state.board) > 0)
            then [HH.div_
                  [HH.text "You are a winner!"]
                  ,HH.button [HE.onClick (HE.input_ $ Reset)] [HH.text "Play again"]]
            else
              (whenNotOnDeck state
                [HH.div [HP.id_ "available"]
                  (foldMap (\piece ->
                    [ HH.div
                      [HP.classes $ HH.ClassName <$> ["placeholder", (pieceClasses piece)]
                      ,HE.onClick (HE.input_ $ GivePiece piece) ]
                      []
                    ]
                  ) (unplayedPieces state.board))
                ,HH.div_ [HH.text "Choose a piece to give to your opponent."]
                ]
              )
            )

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
          [HH.div_
            (whenOnDeck state [HH.div [HP.class_ $ ClassName "play-message"] [HH.text "Play this piece."]])
          ,HH.div
            [HP.classes $ (HH.ClassName <<< (fromMaybe "")) <$> [Just "placeholder-toplay", pieceClasses >>> ((<>) "placeholder") <$> state.ondeck]]
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
            (Right b) -> do
              void $ H.query point $ H.action (GameSpace.PlayPiece piece)
              setOnDeck Nothing
              updateBoard b
            (Left e) -> H.raise $ BadPlay "There is no piece on deck." Nothing point
        Nothing ->
          H.raise $ BadPlay "There is no piece on deck." Nothing point

      pure next

    GivePiece piece next -> do
      setOnDeck (Just piece)
      pure next

    IsPlayed point reply -> do
      state <- H.get
      pure (reply (positionPlayed state.board point))

    Reset next -> do
      updateBoard emptyBoard
      setOnDeck Nothing
      let t = allPoints
      foldM (\a b ->
        H.query b $ H.action (GameSpace.Clear)
      ) Nothing t

      pure next

allPoints = do
  x <- [1,2,3,4]
  y <- [1,2,3,4]
  pure $ Tuple x y



setOnDeck :: forall m r. (MonadState { "ondeck" :: Maybe Piece | r } m) => Maybe Piece -> m Unit
setOnDeck m = do
  H.modify (\s -> s { ondeck = m })

updateBoard :: forall m r. (MonadState { "board" :: Board | r } m) => Board -> m Unit
updateBoard board = do
  H.modify (\s -> s { board = board })
