module Component.Modal where 

import Prelude
import CSS                              as CSS
import Effect.Class.Console             (logShow)
import Effect.Class                     (class MonadEffect)
import Effect.Aff.Class                 (class MonadAff)
import Data.Const                       (Const)
import Data.Maybe                       (Maybe(..))
import Data.Symbol                      (SProxy(..))
import Halogen                          as H
import Halogen.HTML                     as HH
import Halogen.HTML.Events              as HE
import Halogen.HTML.CSS                 as HCSS
import Halogen.Media.Component.Browser  as Browser
import Formless                         as F

import Component.Media                  as Media
import Component.HTML.Utils             (css)
import Data.Image                       (ImageType)
import Resource.Media                   (class ManageMedia)

type Query = Const Void

type ChildSlots = (
  mediaBrowser :: H.Slot (Const Void) (Browser.Output ImageType) Unit
)

data Action
  = Receive Input
  | CloseModal
  | HandleBrowserAction (Browser.Output ImageType)

type Input =
  { isActive :: Boolean
  }

type State =
  { isActive :: Boolean
  }

initialState :: Input -> State
initialState inp = 
  { isActive: inp.isActive
  }

component :: forall m
           . MonadEffect m
          => MonadAff m
          => ManageMedia m
          => H.Component HH.HTML Query Input (Browser.Output ImageType) m
component = 
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
    }
  where
  handleAction :: Action 
               -> H.HalogenM State Action ChildSlots (Browser.Output ImageType) m Unit
  handleAction = case _ of 
    HandleBrowserAction act -> do
      H.raise act
    Receive inp -> do
      H.modify_ _ { isActive = inp.isActive
                  }
    CloseModal  -> do
      state <- H.get
      case state.isActive of
        false -> pure unit
        true  -> H.modify_ _ { isActive = false }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = 
    HH.div
      [ HCSS.style do
        CSS.position CSS.absolute
        CSS.top $ CSS.px 0.0
        CSS.zIndex 99998
        CSS.backgroundColor CSS.black
        case state.isActive of
          true  -> do
            CSS.display CSS.block
          false -> CSS.display CSS.displayNone
      , css "modal-layer" 
      ]
      [ HH.div 
        [ HCSS.style do
          CSS.position CSS.relative
          CSS.zIndex 99999
          CSS.backgroundColor CSS.white
          padding 25.0
        , css "modal-container"
        ] 
        [ HH.div
          [ HCSS.style do
            CSS.position CSS.absolute
            CSS.top $ CSS.px 0.0
            CSS.right $ CSS.px 0.0
            CSS.width $ CSS.px 25.0
            padding 8.0
          , css "modal-header" 
          , HE.onClick \_ -> Just CloseModal
          ]
          [ HH.text "X" ]
        , HH.div
          [ css "modal-body" ]
          [ HH.slot 
            (SProxy :: _ "mediaBrowser")
            unit
            Media.component
            { loadImages: state.isActive }
            (Just <<< HandleBrowserAction)
          ]
        ]
      ]

    where
      padding :: Number -> CSS.CSS
      padding size = CSS.padding 
                     (CSS.px size)
                     (CSS.px size)
                     (CSS.px size)
                     (CSS.px size)
