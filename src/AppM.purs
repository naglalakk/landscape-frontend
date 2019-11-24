module AppM where

import Prelude

import Api.Endpoint as API
import Api.Request (RequestMethod(..), mkRequest)
import Capability.LogMessages (class LogMessages, logMessage)
import Capability.Navigate (class Navigate)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Argonaut (encodeJson, decodeJson)
import Data.BlogPost (BlogPost(..))
import Data.Either (Either(..))
import Data.Environment (Environment(..), Env)
import Data.Log as Log
import Data.Maybe (Maybe(..))
import Data.Route as Route
import Data.URL (BaseURL)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Resource.BlogPost (class ManageBlogPost)
import Resource.Media (class ManageMedia)
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Type.Equality (class TypeEquals, from)

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do 
    env <- ask
    liftEffect case env.environment of
      Production -> pure unit
      _ -> Console.log $ Log.message log

instance navigateAppM :: Navigate AppM where
  navigate = 
    liftEffect <<< setHash <<< print Route.routeCodec 

instance manageBlogPostAppM :: ManageBlogPost AppM where
  getBlogPosts pagination = do
    req <- mkRequest 
      { endpoint: API.BlogPosts pagination
      , method: Get
      }
    case req of
      Just json -> do
        let blogPosts = decodeJson json
        case blogPosts of
          Right bps -> pure bps
          Left err -> do
            logMessage $ Log.Log { message: err }
            pure []
      Nothing -> pure []
  getBlogPost postId = do
    req <- mkRequest
      { endpoint: API.BlogPost postId
      , method: Get
      }
    case req of
      Just json -> do
        let blogPost = decodeJson json
        case blogPost of
          Right bps -> pure $ Just bps
          Left err -> do
            logMessage $ Log.Log { message: err }
            pure Nothing
      Nothing -> pure Nothing
  createBlogPost post = do
    req <- mkRequest
      { endpoint: API.BlogPostCreate
      , method: Post $ Just $ encodeJson post
      }
    case req of
      Just json -> do
        let blogPost = decodeJson json
        case blogPost of
          Right bps -> pure $ Just bps
          Left err -> do
            logMessage $ Log.Log { message: err }
            pure Nothing
      Nothing -> pure Nothing
  updateBlogPost (BlogPost post) = do
    req <- mkRequest
      { endpoint: API.BlogPostUpdate post.id
      , method: Post $ Just $ encodeJson $ BlogPost post
      }
    case req of
      Just json -> do
        let blogPost = decodeJson json
        case blogPost of
          Right bps -> pure $ Just bps
          Left err -> do
            logMessage $ Log.Log { message: err }
            pure Nothing
      Nothing -> pure Nothing
  deleteBlogPost postId = do
    req <- mkRequest
      { endpoint: API.BlogPostDelete postId
      , method: Delete
      }
    pure unit

instance manageMediaAppM :: ManageMedia AppM where
  getImages   pagination = pure []
  uploadImage formData   = pure Nothing

