module Component.HTML.Header where 

import Prelude
import Data.Maybe                           (Maybe(..))
import Halogen.HTML                         as HH
import Halogen.HTML.Events                  as HE
import Halogen.HTML.Properties              as HP
import Halogen.HTML.CSS                     as HCSS

import Component.HTML.Utils                 (css)
import Component.HTML.SocialItem            (socialItem)
import CSS.Utils                            (backgroundCover)

header :: forall props act
        . act 
       -> HH.HTML props act
header darkModeToggle = 
  HH.div
    [ css "header" ]
    [ HH.a
      [ css "title" 
      , HP.href "/"
      ]
      [ HH.text "Donna" ]
    , HH.div
      [ css "menu" ]
      [ HH.div
        [ css "dark-mode-toggle" ]
        [ HH.i
          [ css "far fa-lightbulb" 
          , HE.onClick \_ -> Just darkModeToggle
          ]
          []
        ]
      , HH.ul
        [ css "socials" ]
        [ socialItem "https://github.com/naglalakk" "fab fa-github"
        , socialItem "https://soundcloud.com/donnabotmusic" "fab fa-soundcloud"
        , socialItem "https://www.pinterest.com/k0ttur/" "fab fa-pinterest"
        , socialItem "https://twitter.com/naglalakk" "fab fa-twitter"
        ]
      ]
    ]
