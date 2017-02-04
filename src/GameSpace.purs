module GameSpace where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen (ClassName(..))
import State (Piece, Point, pieceClasses)

type State =
  { piece :: Maybe Piece
  , point :: Point }

data Query a
  = PlayPoint Point a
  | PlayPiece Piece a
  | Clear a
  | Initialize a
  | Finalize a
  | IsPlayed (Maybe Piece -> a)

type Input = (Maybe Piece)

data Message = PlayedPoint Point

gameSpaceHTML piece' props =
  HH.div
    ([ HP.classes $ (map (\a -> ClassName (fromMaybe "" a)) [Just "placeholder", (pieceClasses >>> ((<>) " played ")) <$> piece'])
    ] <> props)
    [ HH.text "" ]

gameSpace :: forall eff. Point -> Maybe Piece -> H.Component HH.HTML Query Input Message (Aff ( console :: CONSOLE | eff))
gameSpace ipoint ipiece =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Just (H.action Finalize)
    , receiver: (const Nothing)
    }
  where

  -- reciever :: Input -> Maybe (Query Message (Aff (console :: CONSOLE | eff)))
  reciever p =
    case p of
      (Just pi) -> Just $ (H.action $ PlayPiece pi)
      Nothing -> Nothing

  initialState :: State
  initialState = { point : ipoint, piece : ipiece }


  render :: State -> H.ComponentHTML Query
  render { piece : mpiece, point : point } =
    case (mpiece) of
      Just _ -> gameSpaceHTML mpiece []
      Nothing -> gameSpaceHTML Nothing [HE.onClick (HE.input_ $ PlayPoint point) ]



  eval :: Query ~> H.ComponentDSL State Query Message (Aff ( console :: CONSOLE | eff))
  eval = case _ of
    Initialize next -> do
      liftAff $ log $ "Initializing GameSpace"
      pure next

    Finalize next -> do
      pure next

    PlayPoint point next -> do
      liftAff $ log $ "(GameSpace) Playing point" <> (show point)
      H.raise $ PlayedPoint point
      pure next

    PlayPiece piece' next -> do
      liftAff $ log $ "(GameSpace) Playing at " <> (show ipoint) <> " " <> (show piece')
      H.modify \s -> s { piece = (Just piece') }
      pure next

    IsPlayed reply -> do
      state <- H.get
      pure (reply state.piece)

    Clear next -> do
      H.modify \s -> s { piece = Nothing }
      pure next
