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
  initialState = 
    { 
    }
  
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
        , HH.div
          [ css "breathe" ]
          []
        , HH.div
          [ css "about-section" ]
          [ HH.p
            []
            [ HH.text "Hi. I'm Donna - Artist / Programmer / Composer / Cat owner."
            ]
          , HH.p
            []
            [ HH.text "I live in Reykjavík, Iceland where I work as a freelance programmer / consultant. I love building things and occasionally I will put them up here."
            ]
          , HH.p
            []
            [ HH.text "If you want to support my already pretty serious caffeine addiction, you can buy me a "
            , HH.a
              [ HP.href "https://ko-fi.com/donna0648" ]
              [ HH.text "Ko-fi." ]
            ]
          , HH.p
            []
            [ HH.text "If you want to support my personal projects, research, resources on functional programming and hacking in the Cardano space, feel free to leave me a tip at my ADA address"
            ]
          , HH.p
            [ css "address-string" ]
            [ HH.text "addr1q9rxlsrptvxqk5xmfdtdcgqf3ngmq674mncusxr6fw0s5ms8f9xeqlwc9melq7t4gfgwhpzzd0ug2czg6zr2w5c2ssxsduskdl" ]
          ]
        ]
      ]
