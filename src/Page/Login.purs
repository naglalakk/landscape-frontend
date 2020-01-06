module Page.Login where

import Prelude
import Data.Const                   (Const)
import Data.Maybe                   (Maybe(..))
import Data.Symbol                  (SProxy(..))
import Effect.Aff.Class             (class MonadAff)
import Halogen                      as H
import Halogen.HTML                 as HH
import Formless                     as F

import Component.Utils              (OpaqueSlot)
import Component.HTML.Utils         (css)
import Data.User                    (Auth)
import Form.Login                   as LoginForm

type State = {}

data Action 
  = HandleLoginForm Auth

type ChildSlots = (
  loginForm :: H.Slot (F.Query LoginForm.LoginForm LoginForm.Query LoginForm.ChildSlots) Auth Unit
)
type Query = Const Void

initialState :: State
initialState = {}

component :: forall m
           . Monad m
          => MonadAff m
          => H.Component HH.HTML Query Unit Void m
component =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      }
    }
  where
  handleAction = case _ of
    HandleLoginForm auth -> pure unit

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div 
      [] 
      [ loginSlot 
      ]

    where
      loginSlot =
        HH.slot
        (SProxy :: SProxy "loginForm")
        unit
        (LoginForm.component)
        unit
        (Just <<< HandleLoginForm)
