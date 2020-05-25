module Main where

import Prelude
import Affjax                       (request)
import Control.Monad.Error.Class    (class MonadError)
import Data.Argonaut                (encodeJson, decodeJson)
import Data.Bifunctor               (bimap)
import Data.Either                  (Either(..), hush)
import Data.Foldable                (traverse_)
import Data.Maybe                   (Maybe(..))
import Data.String                  (drop)
import Effect                       (Effect)
import Effect.Class.Console         (logShow)
import Effect.Aff                   (Aff, launchAff_)
import Effect.Aff.Class             (class MonadAff)
import Effect.Aff.Bus               as Bus
import Effect.Ref                   as Ref
import Foreign                      as Foreign
import Halogen                      as H
import Halogen.Aff                  as HA
import Halogen.HTML                 as HH
import Halogen.VDom.Driver          (runUI)
import Routing.Duplex               (parse)
import Routing.PushState            (makeInterface, matchesWith)
import Simple.JSON                  (read_)

import AppM                         (runAppM)
import Api.Request                  (RequestMethod(..)
                                    ,defaultRequest)
import Api.Endpoint                 as API
import Component.Router             as Router
import Config                       (environment, apiURL)
import Data.Auth                    (APIAuth(..)
                                    ,readToken)
import Data.Environment             (Env
                                    ,UserEnv
                                    ,toEnvironment)
import Data.Route                   (routeCodec)
import Data.User                    (User)
import Data.URL                     (BaseURL(..))

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  
  currentUser <- H.liftEffect $ Ref.new Nothing
  userBus <- H.liftEffect Bus.make
  interface <- H.liftEffect makeInterface 

  H.liftEffect readToken >>= traverse_ \token -> do
    let 
      requestOptions = 
        { endpoint: API.UserLogin
        , method: Get 
        , auth: Just $ Basic token
        }
    res <- H.liftAff $ request $ defaultRequest (BaseURL apiURL) requestOptions

    case (hush res.body) of
      Just json -> do
        let 
          user = (decodeJson json) :: Either String User
        case user of
          Right u -> H.liftEffect $ Ref.write (Just u) currentUser
          Left err -> pure unit
      Nothing -> pure unit

  let 
    environ = toEnvironment environment
    url     = BaseURL apiURL
    env :: Env
    env = 
      { environment: environ
      , apiURL: url
      , userEnv: userEnv
      , pushInterface: interface
      }
      where
        userEnv :: UserEnv
        userEnv = { currentUser, userBus }

    rootComponent :: H.Component HH.HTML Router.Query Unit Void Aff
    rootComponent = H.hoist (runAppM env) Router.component

  halogenIO <- runUI rootComponent unit body

  void $ H.liftEffect $ interface.listen \location -> do
    let 
      state = (read_ location.state) :: (Maybe { old :: String })
    case state of 
      Just st -> do
        let
          new  = hush $ parse routeCodec $ drop 1 location.pathname
          old  = hush $ parse routeCodec $ st.old
        when (old /= new) do
          case new of
            Just r -> launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate r
            Nothing -> pure unit
      Nothing -> pure unit
  pure unit
