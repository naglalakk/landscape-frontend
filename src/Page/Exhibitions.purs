module Page.Exhibitions where

import Prelude

import Capability.Navigate (class Navigate, navigate)
import Component.HTML.Exhibition (exhibition)
import Component.HTML.Utils (css, maybeElem)
import Data.Array (head, tail)
import Data.Exhibition (Exhibition(..), dateTimeFormat)
import Data.Formatter.DateTime (format)
import Data.Image (Image(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.PreciseDateTime (PreciseDateTime(..))
import Data.Route (Route)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Resource.Exhibition (class ManageExhibition, allExhibitions)
import Timestamp (Timestamp(..))

type State = 
  { exhibitions :: Array Exhibition
  }

data Action 
  = Initialize
  | NavigateAction Route

component :: forall q i o m
           . ManageExhibition m
          => Navigate m 
          => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction 
      , initialize = Just Initialize
      }
    }
  where
  initialState :: State
  initialState = 
    { exhibitions: []
    }
  
  handleAction :: Action 
               -> H.HalogenM State Action () o m Unit
  handleAction = case _ of
    Initialize -> do
      exs <- allExhibitions
      H.modify_ _ { exhibitions = exs }
    NavigateAction route -> navigate route

  render :: State -> H.ComponentHTML Action () m
  render state = 
    HH.div
      [ css "page-exhibitions container" ]
      [ HH.div
        [ css "container-padding" ]
        [ HH.h1
          [ css "underlined" ]
          [ HH.text "Exhibitions" ]
        , HH.div
          [ css "exhibitions flex" ]
          ([ maybeElem featured \ex ->
            exhibition NavigateAction ex "featured"
          ] <> (map (\ex ->
              exhibition NavigateAction ex ""
              ) rest))
        ]
      ]

    where
      featured = head state.exhibitions
      rest = fromMaybe [] (tail state.exhibitions)
