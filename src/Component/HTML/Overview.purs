module Component.HTML.Overview where

import Prelude

import Component.HTML.Utils (css, maybeElem)
import Data.Image (Image(..))
import Data.Item (Item(..), ItemId(..))
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

overview :: forall props act
          . act
         -> (ItemId -> act)
         -> Boolean
         -> Array Item
         -> HH.HTML props act
overview closeAction selectAction showOverview items = 
  HH.div
    [ css $ "overview show-" <> show showOverview ]
    [ HH.div
      [ css "relative" ]
      [ HH.div
        [ HE.onClick \_ -> Just closeAction ]
        []
      ]
    , HH.div
      [ css "artworks-container container" ]
      [ HH.div
        [ css "container-padding flex space-between" ]
        (map (\(Item item) -> 
          HH.div
            [ css "artwork" 
            , HE.onClick \_ -> Just $ selectAction item.id
            ]
            [ maybeElem item.image \(Image img) ->
              maybeElem img.thumbnail \thumb -> 
                HH.img
                  [ HP.src thumb ]
            ]
          ) items)
      ]
    ]
