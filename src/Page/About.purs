module Page.About where

import Prelude

import Component.HTML.Utils (css)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = 
  {
  }

data Action 
  = NoAction

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction 
      }
    }
  where
  initialState :: State
  initialState = {}
  
  handleAction :: Action 
               -> H.HalogenM State Action () o m Unit
  handleAction = case _ of
    _ -> pure unit

  render :: State -> H.ComponentHTML Action () m
  render state = 
    HH.div
      [ css "page-about container border-white" ]
      [ HH.div
        [ css "container-padding" ]
        [ HH.h1
          [ css "underlined" ]
          [ HH.text "About" ]
        ]
      ]
