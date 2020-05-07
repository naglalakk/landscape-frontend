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
      ]
      [ HH.video
        [ HH.attr (HH.AttrName "autoplay") "true"
        , HH.attr (HH.AttrName "loop") "true"
        , HH.attr (HH.AttrName "muted") "true"
        ]
        [ HH.source
          [ HP.src "img/profile.mp4" 
          , HH.attr (HH.AttrName "type") "video/mp4"
          ]
        , HH.source
          [ HP.src "img/profile.webm" 
          , HH.attr (HH.AttrName "type") "video/webm"
          ]
        ]
      ]
    , HH.h2
      [ css "title" ]
      [ HH.text "Donna" ]
    , HH.ul
      [ css "socials" ]
      [ socialItem "https://github.com/naglalakk" "fab fa-github"
      , socialItem "https://soundcloud.com/donnabotmusic" "fab fa-soundcloud"
      , socialItem "https://www.pinterest.com/k0ttur/" "fab fa-pinterest"
      , socialItem "https://twitter.com/naglalakk" "fab fa-twitter"
      ]
    , HH.div
      [ css "line" ]
      []
    ]
