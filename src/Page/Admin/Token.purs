module Page.Admin.Token where

import Prelude

import Capability.Navigate (class Navigate, navigate)
import Data.Const (Const(..))
import Data.Functor.Variant (SProxy(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Route (Route(..))
import Data.Token (Token(..), TokenId(..))
import Effect.Aff.Class (class MonadAff)
import Form.Admin.TokenForm as TokenForm
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Resource.Token (class ManageToken, getTokenAmount, getTokenById, createToken, updateToken)

type Input = 
  { tokenId :: TokenId
  }

type State = 
  { tokenId :: TokenId
  , token   :: Maybe Token
  }

type ChildSlots =
  ( formless :: H.Slot (F.Query TokenForm.TokenForm (Const Void) TokenForm.ChildSlots) Token Unit
  )

data Action 
  = Initialize
  | HandleTokenForm Token

component :: forall q o m
           . ManageToken m
          => MonadAff m
          => Navigate m
          => H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction 
      , initialize = Just Initialize
      }
    }
  where
  initialState :: Input -> State
  initialState inp = 
    { tokenId: inp.tokenId
    , token: Nothing
    }
  
  handleAction :: Action 
               -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      if state.tokenId == TokenId 0 
        then pure unit 
        else do
          token <- getTokenById state.tokenId
          -- Get the amount separately
          tokenAmount <- getTokenAmount state.tokenId
          let
            finalToken = 
              map (\(Token tkn) -> 
                Token tkn { amount = fromMaybe 0 tokenAmount }) token
          H.modify_ _ { token = finalToken }

    HandleTokenForm (Token token) -> case token.id of
      TokenId 0 -> do
        newToken <- createToken $ Token token
        case newToken of 
          Just (Token nToken) -> 
            navigate $ AdminToken nToken.id
          Nothing -> pure unit
      _ -> do
        _ <- updateToken $ Token token
        pure unit

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = 
    HH.div_
      [ HH.h1
        []
        [ HH.text "Token" ]
      , HH.slot
        (SProxy :: _ "formless") 
        unit
        TokenForm.component
        { token: state.token }
        (Just <<< HandleTokenForm)
      ]
