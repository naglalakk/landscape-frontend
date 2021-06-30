module Page.Login where

import Prelude

import Api.Utils (authenticate)
import CSS.Shared (button, loginForm)
import Capability.Navigate (class Navigate, navigate)
import Component.HTML.Utils (css)
import Component.Utils (OpaqueSlot)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Auth (APIAuth(..), UserAuth(..), base64encode, writeToken)
import Data.Const (Const)
import Data.Environment (UserEnv(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Route as R
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Form.Login as LoginForm
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Resource.User (class ManageUser, loginUser)

type State =
  { redirect :: Boolean }

type Input =
  { redirect :: Boolean }

data Action 
  = HandleLoginForm UserAuth

type ChildSlots = (
  loginForm :: H.Slot (F.Query LoginForm.LoginForm LoginForm.Query LoginForm.ChildSlots) UserAuth Unit
)
type Query = Const Void

component :: forall m r
           . Monad m
          => MonadAff m
          => MonadAsk { userEnv :: UserEnv | r } m
          => ManageUser m
          => Navigate m
          => H.Component HH.HTML Query Input Void m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      }
    }
  where
  handleAction = case _ of
    HandleLoginForm auth -> do
      env <- asks _.userEnv
      currentUser <- authenticate env auth
      case currentUser of
        Just user -> navigate $ R.AdminHome
        Nothing   -> pure unit

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div 
      [] 
      [ HCSS.stylesheet do
        loginForm
        button
      , loginSlot 
      ]

    where
      loginSlot =
        HH.slot
        (SProxy :: SProxy "loginForm")
        unit
        (LoginForm.component)
        unit
        (Just <<< HandleLoginForm)
