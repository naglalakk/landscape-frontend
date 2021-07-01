module Component.HTML.Exhibition where

import Prelude

import Component.HTML.Utils (css, maybeElem)
import Data.Exhibition (dateTimeFormat)
import Data.Exhibition as E
import Data.Formatter.DateTime (format)
import Data.Image (Image(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.PreciseDateTime (PreciseDateTime(..))
import Data.Route as R
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Timestamp (Timestamp(..))

exhibition :: forall props act
            . (R.Route -> act)
           -> E.Exhibition
           -> String
           -> HH.HTML props act
exhibition navigateAction (E.Exhibition ex) addedCls =
  HH.div
    [ css $ "exhibition relative " <> addedCls
    , HE.onClick \_ -> Just $ navigateAction $ R.Exhibition ex.id
    , HP.attr (HH.AttrName "style") 
      ("background-image: url('" <> 
      (fromMaybe "" (map (\(Image img) -> img.src) ex.featuredImage)) <> "')")
    ]
    [ HH.div
      [ css "info" ]
      [ HH.p
        [ css "title" ]
        [ HH.text ex.title ]
      , maybeElem ex.startDate \(Timestamp (PreciseDateTime sDate nano)) ->
        HH.span
          [ css "start-date" ]
          [ HH.text $ format E.dateTimeFormat sDate ]
      , maybeElem ex.endDate \(Timestamp (PreciseDateTime eDate nano)) ->
        HH.span
          []
          [ HH.text $ " - " <> format E.dateTimeFormat eDate
          ]
      ]
    ]
