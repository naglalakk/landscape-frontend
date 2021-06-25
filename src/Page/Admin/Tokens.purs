module Page.Admin.Tokens where

import Prelude

import Capability.Navigate (class Navigate, navigate)
import Component.HTML.Utils (css)
import Component.Table as Table
import Data.Const (Const(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Route as R
import Data.Symbol (SProxy(..))
import Data.Token (Token(..), TokenId(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Resource.Token (class ManageToken, allTokens)

type State = 
  { tokens :: Array Token
  }

data Action 
  = Initialize
  | HandleTableAction Table.Output
  | NavigateAction R.Route

type ChildSlots = (
  table :: H.Slot (Const Void) Table.Output Unit
)

component :: forall q i o m
           . MonadAff m
          => MonadEffect m
          => ManageToken m
          => Navigate m
          => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction 
      , initialize = Just Initialize
      }
    }
  where
  initialState :: State
  initialState = 
    { tokens: []
    }
  
  handleAction :: Action 
               -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      tokens <- allTokens
      H.modify_ _ { tokens = tokens }
    HandleTableAction act -> pure unit

    NavigateAction route -> navigate route

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = 
    HH.div_ 
      [ HH.h1
        []
        [ HH.text "Tokens" ]
      , HH.a
        [ css "button" 
        , HE.onClick \_ -> Just $ NavigateAction $ R.AdminToken $ TokenId 0
        ]
        [ HH.text "+ New Token" ]
      , HH.slot
        (SProxy :: _ "table")
        unit
        Table.component
        { rows: map (\(Token token) -> { id: unwrap token.id, label: token.title }) state.tokens
        , headers: ["Title", "Update", "Delete"]
        }
        (Just <<< HandleTableAction)
      ]
