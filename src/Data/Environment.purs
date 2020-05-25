module Data.Environment where 

import Prelude
import Data.Maybe           (Maybe(..))
import Effect.Aff.Bus       (BusRW)
import Effect.Ref           (Ref)
import Routing.PushState    (PushStateInterface)

import Data.URL         (BaseURL(..))
import Data.User        (User)

data Environment 
  = Development
  | Staging
  | Production

derive instance eqEnvironment :: Eq Environment
derive instance ordEnvironment :: Ord Environment

type Env =
  { environment :: Environment
  , apiURL      :: BaseURL
  , userEnv     :: UserEnv
  , pushInterface :: PushStateInterface
  }

type UserEnv =
  { currentUser :: Ref (Maybe User)
  , userBus     :: BusRW (Maybe User)
  }

toEnvironment :: String -> Environment
toEnvironment "Production"  = Production
toEnvironment "Staging"     = Staging
toEnvironment _             = Development

getSiteURL :: Environment -> String
getSiteURL Production   = "https://donnabot.dev"
getSiteURL Staging      = "https://staging.donnabot.dev"
getSiteURL _            = "https://donnabot.dev"
