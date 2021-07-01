module Component.HTML.Header where 

import Prelude

import CSS.Utils (backgroundCover)
import Component.HTML.SocialItem (socialItem)
import Component.HTML.Utils (css)
import Data.Maybe (Maybe(..))
import Data.Route (Route(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

header :: forall props act
        . (Route -> act)
       -> HH.HTML props act
header navigateAction = 
  HH.div
    [ css "header flex align-center space-between" ]
    [ HH.div 
      []
      []
    , HH.div
      [ css "menu flex" ]
      [ HH.a
        [ HE.onClick \_ -> Just $ navigateAction Home
        , css "menu-item"
        ]
        [ HH.text "blog" ]
      , HH.span
        [ css "divider" ]
        [ HH.text "/" ]
      , HH.a
        [ HE.onClick \_ -> Just $ navigateAction Exhibitions
        , css "menu-item"
        ]
        [ HH.text "exhibitions"
        ]
      , HH.span
        [ css "divider" ]
        [ HH.text "/" ]
      , HH.a
        [ HE.onClick \_ -> Just $ navigateAction About
        , css "menu-item"
        ]
        [ HH.text "about"
        ]
      ]
    ]
