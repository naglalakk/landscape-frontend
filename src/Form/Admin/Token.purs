module Form.Admin.TokenForm where

import Prelude

import Component.HTML.Utils (css, withLabel)
import Data.Const (Const(..))
import Data.Int (decimal, fromString, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Data.Token (Token(..), TokenId(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Form.Error (FormError)
import Form.Validation (validateStr)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Timestamp (Timestamp(..))

newtype TokenForm r f = TokenForm (r
  ( id :: f Void TokenId TokenId
  , title :: f FormError String String
  , policyId :: f Void (Maybe String) (Maybe String)
  , amount :: f Void Int Int
  , quantity :: f Void Int Int
  , minted :: f Void Int Int
  , available :: f Void Int Int
  , metadata :: f Void (Maybe String) (Maybe String)
  , createdAt :: f Void Timestamp Timestamp
  , updatedAt :: f Void (Maybe Timestamp) (Maybe Timestamp)
  ))

derive instance newtypeTokenForm :: Newtype (TokenForm r f) _

prx :: F.SProxies TokenForm
prx = F.mkSProxies (F.FormProxy :: F.FormProxy TokenForm)

type Input = 
  { token :: Maybe Token
  }

data Action
  = Initialize
  | Receive Input

type ChildSlots = ()
type Query = Const Void

type AddedState = ()

component :: forall m
        . MonadAff m
      => MonadEffect m
      => F.Component TokenForm Query ChildSlots Input Token m
component = F.component input F.defaultSpec
  { render = render
  , handleEvent = handleEvent
  , handleAction = handleAction
  , initialize = Just Initialize
  , receive = Just <<< Receive
  }
  where
  input inp = 
    { initialInputs: case inp.token of
      Just (Token token) ->
        Just $ F.wrapInputFields token
      Nothing -> Nothing
    , validators: TokenForm
      { id: F.noValidation
      , title: validateStr
      , policyId: F.noValidation
      , amount: F.noValidation
      , quantity: F.noValidation
      , minted: F.noValidation
      , available: F.noValidation
      , metadata: F.noValidation
      , createdAt: F.noValidation
      , updatedAt: F.noValidation
      }
    }

  handleAction :: Action
               -> F.HalogenM TokenForm AddedState Action ChildSlots Token m Unit
  handleAction = case _ of
    Initialize -> pure unit
    Receive inp -> do
      case inp.token of
        Just (Token token) -> do
          eval $ F.setValidate prx.id token.id
          eval $ F.setValidate prx.title token.title
          eval $ F.setValidate prx.policyId token.policyId
          eval $ F.setValidate prx.amount token.amount
          eval $ F.setValidate prx.quantity token.quantity
          eval $ F.setValidate prx.minted token.minted
          eval $ F.setValidate prx.available token.available
          eval $ F.setValidate prx.metadata token.metadata
          eval $ F.setValidate prx.createdAt token.createdAt
          eval $ F.setValidate prx.updatedAt token.updatedAt
        Nothing -> pure unit
    where
      eval act = F.handleAction handleAction handleEvent act

  handleEvent :: F.Event TokenForm AddedState
              -> F.HalogenM TokenForm AddedState Action ChildSlots Token m Unit
  handleEvent = case _ of
    F.Submitted form -> do
      let 
        fields = F.unwrapOutputFields form
      H.raise $ Token fields
    _ -> pure unit

  render :: F.PublicState TokenForm AddedState
         -> F.ComponentHTML TokenForm Action ChildSlots m
  render st =
    HH.div_ 
      [ withLabel "Title" (HH.input
        [ css "text-input"
        , HP.value $ F.getInput prx.title st.form
        , HE.onValueInput $ Just <<< F.setValidate prx.title
        ])
      , withLabel "PolicyId" (HH.input
        [ css "text-input"
        , HP.value $ fromMaybe "" $ F.getInput prx.policyId st.form
        , HE.onValueInput (\str -> Just $ F.setValidate prx.policyId $ Just str)
        ])
      , withLabel "Amount" (HH.input
        [ HP.type_ HP.InputNumber
        , HP.value $ toStringAs decimal $ F.getInput prx.amount st.form
        , HE.onValueInput (\str -> Just $ F.setValidate prx.amount (fromMaybe 0 $ fromString str))
        ])
      , withLabel "Quantity" (HH.input
        [ HP.type_ HP.InputNumber
        , HP.value $ toStringAs decimal $ F.getInput prx.quantity st.form
        , HE.onValueInput (\str -> Just $ F.setValidate prx.quantity (fromMaybe 0 $ fromString str))
        ])
      , withLabel "Metadata" (HH.textarea
        [ HP.value $ fromMaybe "" $ F.getInput prx.metadata st.form
        , HE.onValueInput (\str -> Just <<< F.setValidate prx.metadata  $ Just str)
        , HP.rows 10
        , HP.cols 60
        ])
      , HH.button
        [ css "button" 
        , HE.onClick \_ -> Just F.submit
        ]
        [ HH.text "Save" ]
      ]
