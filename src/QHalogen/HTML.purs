module QHalogen.HTML where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as HES
import Halogen.HTML (HTML(..))

verticalButtonList :: forall a b. Array (HTML a b) -> HTML a b
verticalButtonList buttons =
  HH.ul [HP.class_ (H.ClassName "vertical-button-list")] (map (\button -> HH.li_ [button] ) buttons)
