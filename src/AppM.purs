module AppM where

import Prelude

import Control.Monad.Reader.Trans   (class MonadAsk
                                    ,ReaderT
                                    ,ask, asks, runReaderT)
import Data.Argonaut                (encodeJson, decodeJson)
import Data.Either                  (Either(..))
import Data.Environment             (Environment(..), Env)
import Data.Maybe                   (Maybe(..))
import Effect.Aff                   (Aff)
import Effect.Aff.Class             (class MonadAff)
import Effect.Class                 (class MonadEffect
                                    ,liftEffect)
import Effect.Console               as Console
import Routing.Duplex               (print)
import Routing.Hash                 (setHash)
import Type.Equality                (class TypeEquals, from)
import Web.HTML                     (window)
import Web.HTML.Window              as Window
import Web.HTML.Location            (setHref, Location)
import Slug                         as Slug

import Api.Endpoint                 as API
import Api.Request                  (RequestMethod(..)
                                    ,FormDataRequestMethod(..)
                                    ,mkRequest
                                    ,mkFormDataRequest)
import Capability.LogMessages       (class LogMessages
                                    ,logMessage)
import Capability.Navigate          (class Navigate)
import Data.Auth                    (APIAuth(..)
                                    ,Password(..)
                                    ,apiAuth
                                    ,base64encodeUserAuth)
import Data.BlogPost                (BlogPost(..))
import Data.Image                   (decodeImageArray)
import Data.Log                     as Log
import Data.Route                   as Route
import Data.User                    (Username(..))
import Data.URL                     (BaseURL)
import Resource.BlogPost            (class ManageBlogPost)
import Resource.Media               (class ManageMedia)
import Resource.User                (class ManageUser)


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
  navigateForm route = do
    w <- liftEffect window
    location <- liftEffect $ Window.location w
    let href = ("?#" <> (print Route.routeCodec route))
    liftEffect $ setHref href location

instance manageBlogPostAppM :: ManageBlogPost AppM where
  getBlogPosts pagination = do
    req <- mkRequest 
      { endpoint: API.BlogPosts pagination
      , method: Get
      , auth: Nothing
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
      , auth: Nothing
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

  getBlogPostBySlug slug = do
    req <- mkRequest
      { endpoint: API.BlogPostBySlug $ Slug.toString slug
      , method: Get
      , auth: Nothing
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
      , auth: Just apiAuth
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
      , auth: Just apiAuth
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
      , auth: Just apiAuth
      }
    pure unit

instance manageMediaAppM :: ManageMedia AppM where
  getImages   pagination = do
    req <- mkRequest
      { endpoint: API.Images pagination
      , method: Get
      , auth: Nothing
      }
    case req of
      Just json -> do
        let images = decodeImageArray json
        case images of
          Right i -> pure i
          Left err -> do
            logMessage $ Log.Log { message: err }
            pure []
      Nothing -> pure []

  uploadImage formData = do
    req <- mkFormDataRequest
      { endpoint: API.ImageUpload
      , method: PostFormData $ Just formData
      , auth: Just apiAuth
      }
    case req of
      Just json -> do
        let img = decodeJson json
        case img of
          Right i -> pure $ Just i
          Left err -> do
            logMessage $ Log.Log { message: err }
            pure Nothing
      Nothing -> pure Nothing

instance manageUserAppM :: ManageUser AppM where
  loginUser auth = do
    req <- mkRequest 
      { endpoint: API.UserLogin
      , method: Get
      , auth: Just $ Basic $ base64encodeUserAuth auth
      }
    case req of
      Just json -> do
        let user = decodeJson json
        case user of
          Right u -> pure $ Just u
          Left err -> do
            logMessage $ Log.Log { message: err }
            pure Nothing
      Nothing -> pure Nothing
