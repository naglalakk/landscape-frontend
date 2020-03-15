module Component.HTML.Header where 

import Prelude
import Halogen.HTML                         as HH
import Halogen.HTML.Properties              as HP
import Halogen.HTML.CSS                     as HCSS

import Component.HTML.Utils                 (css)
import Component.HTML.SocialItem            (socialItem)
import CSS.Utils                            (backgroundCover)

header :: forall i p. HH.HTML i p
header = 
  HH.div
    [ css "header" ]
    [ HH.div
      [ css "profile-image" 
      , HCSS.style $ backgroundCover "img/profile.gif"
      ]
      [
      ]
    , HH.h2
      [ css "title" ]
      [ HH.text "Donna" ]
    , HH.ul
      [ css "socials" ]
      [ socialItem "https://github.com/naglalakk" "fab fa-github"
      , socialItem "https://soundcloud.com/donnainternational" "fab fa-soundcloud"
      , socialItem "https://www.pinterest.com/k0ttur/" "fab fa-pinterest"
      , socialItem "https://twitter.com/naglalakk" "fab fa-twitter"
      ]
    , HH.div
      [ css "line" ]
      []
    ]
