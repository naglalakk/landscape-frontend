module Data.Environment where 

import Prelude
import Data.Maybe       (Maybe(..))
import Effect.Aff.Bus   (BusRW)
import Effect.Ref       (Ref)

import Data.URL         (BaseURL(..))
import Data.User        (User)

data Environment 
  = Development
  | Production

derive instance eqEnvironment :: Eq Environment
derive instance ordEnvironment :: Ord Environment

type Env =
  { environment :: Environment
  , apiURL      :: BaseURL
  , userEnv     :: UserEnv
  }

type UserEnv =
  { currentUser :: Ref (Maybe User)
  , userBus     :: BusRW (Maybe User)
  }

toEnvironment :: String -> Environment
toEnvironment = case _ of
  "Production" -> Production
  _            -> Development
