module Component.HTML.Admin where

import Prelude

import Halogen.HTML             as HH
import Halogen.HTML.Properties  as HP

import Data.Route               (Route(..))
import Component.HTML.Utils     (css, safeHref)

menuLink :: forall i p. Route -> String -> HH.HTML i p
menuLink route txt = 
  HH.li
    [ css "admin-menu-item" ]
    [ HH.a
      [ safeHref route ]
      [ HH.text txt ]
    ]

adminMenu :: forall i p. HH.HTML i p
adminMenu =
  HH.div
    [ css "admin-menu" ]
    [ HH.ul
      []
      [ menuLink AdminHome "Home" 
      , menuLink AdminBlogPosts "Posts"
      ]
    ]

withAdmin :: forall i p. HH.HTML i p -> HH.HTML i p
withAdmin content = 
  HH.div
    [ css "admin-container" ]
    [ adminMenu
    , HH.div
      [ css "content" ]
      [ content ]
    ]
