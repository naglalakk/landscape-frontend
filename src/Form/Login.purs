module Form.Login where 

import Prelude
import Data.Const                   (Const(..))
import Data.Maybe                   (Maybe(..))
import Data.Newtype                 (class Newtype)
import Effect.Aff.Class             (class MonadAff)
import Form.Error                   (FormError(..))
import Form.Validation              (validateUsername, validateStr)
import Formless                     as F
import Halogen                      as H
import Halogen.HTML                 as HH
import Halogen.HTML.Events          as HE
import Halogen.HTML.Properties      as HP

import Component.HTML.Utils         (withLabel, css)
import Data.User                    (Auth(..), Username(..))

newtype LoginForm r f = LoginForm (r
  ( username :: f FormError String Username
  , password :: f FormError String String
  ))

derive instance newtypeLoginForm :: Newtype (LoginForm r f) _

prx :: F.SProxies LoginForm 
prx = F.mkSProxies (F.FormProxy :: F.FormProxy LoginForm)

type Input      = Unit
type Query      = Const Void
type Output     = Auth
type ChildSlots = ()


component :: forall m
           . Monad m
          => MonadAff m
          => F.Component LoginForm Query ChildSlots Input Output m
component = F.component (const input) F.defaultSpec
  { render = render
  , handleEvent = handleEvent
  }
  where

  input :: F.Input' LoginForm m
  input = 
    { initialInputs: Nothing
    , validators: LoginForm
      { username: validateUsername
      , password: validateStr
      }
    }

  handleEvent = case _ of
    F.Submitted outputs -> H.raise $ Auth $ F.unwrapOutputFields outputs
    _ -> pure unit

  render st = 
    HH.form_ 
      [ withLabel "Username*" (HH.input
        [ css "text-input"
        , HE.onValueInput $ Just <<< F.setValidate prx.username
        ])
      , withLabel "Password*" (HH.input
        [ css "text-input"
        , HE.onValueInput $ Just <<< F.setValidate prx.password
        , HP.type_ HP.InputPassword
        ])
      ]
