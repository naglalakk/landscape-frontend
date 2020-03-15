module Component.HTML.SocialItem where 

import Prelude
import Halogen.HTML                         as HH
import Halogen.HTML.Properties              as HP

import Component.HTML.Utils                 (css)

socialItem :: forall i p. String -> String -> HH.HTML i p
socialItem link icon = 
  HH.li
    [ css "social-item" ]
    [ HH.a
      [ HP.href link ]
      [ HH.i
        [ css icon ]
        []
      ]
    ]
