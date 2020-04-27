module Main where

import Prelude
import Affjax                       (request)
import Control.Monad.Error.Class    (class MonadError)
import Data.Argonaut                (encodeJson, decodeJson)
import Data.Bifunctor               (bimap)
import Data.Either                  (Either(..), hush)
import Data.Foldable                (traverse_)
import Data.Maybe                   (Maybe(..))
import Effect                       (Effect)
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
import Routing.Hash                 (matchesWith)

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

  let 
    environ = toEnvironment environment
    url     = BaseURL apiURL
    

    env :: Env
    env = 
      { environment: environ
      , apiURL: url
      , userEnv: userEnv
      }
      where
        userEnv :: UserEnv
        userEnv = { currentUser, userBus }

    rootComponent :: H.Component HH.HTML Router.Query Unit Void Aff
    rootComponent = H.hoist (runAppM env) Router.component

  halogenIO <- runUI rootComponent unit body

  void $ H.liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new
  
  pure unit
