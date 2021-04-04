module AppM where

import Prelude

import Affjax (printError)
import Api.Endpoint as API
import Api.Request (RequestMethod(..), FormDataRequestMethod(..), mkRequest, mkFormDataRequest)
import Capability.LogMessages (class LogMessages, logMessage)
import Capability.Navigate (class Navigate)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Argonaut (JsonDecodeError, decodeJson, encodeJson, printJsonDecodeError)
import Data.Auth (APIAuth(..), apiAuth, base64encodeUserAuth)
import Data.BlogPost (BlogPost(..))
import Data.Either (Either(..))
import Data.Environment (Environment(..), Env)
import Data.Image (decodeImageArray)
import Data.Log as Log
import Data.Maybe (Maybe(..))
import Data.Route as Route
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console as Console
import Elasticsearch.Client (SearchResponse(..), SearchHit(..))
import Resource.BlogPost (class ManageBlogPost)
import Resource.Media (class ManageMedia)
import Resource.Tag (class ManageTag)
import Resource.User (class ManageUser)
import Routing.Duplex (print)
import Simple.JSON (write)
import Slug as Slug
import Type.Equality (class TypeEquals, from)
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window as Window


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

genericListRequest endpoint method auth = do
  req <- mkRequest
    { endpoint: endpoint
    , method: method
    , auth: auth
    }
  case req of
    Right json -> do
      let et = decodeJson json.body
      case et of
        Right e -> pure e
        Left err -> do
          logMessage $ printJsonDecodeError err
          pure []
    Left err -> do
      logMessage $ printError err
      pure []

genericRequest endpoint method auth = do
  req <- mkRequest
    { endpoint: endpoint
    , method: method
    , auth: auth
    }
  case req of
    Right json -> do
      let et = decodeJson json.body
      case et of
        Right e -> pure $ Just e
        Left err -> do
          logMessage $ printJsonDecodeError err
          pure Nothing
    Left err -> do
      logMessage $ printError err
      pure Nothing

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do 
    env <- ask
    liftEffect case env.environment of
      Production -> pure unit
      _ -> Console.log log

instance navigateAppM :: Navigate AppM where
  navigate route = do
    -- Get our PushStateInterface instance from env
    env <- ask
    let 
      href = "/" <> (print Route.routeCodec route)
    logShow href
    -- pushState new destination
    liftEffect $ 
      env.pushInterface.pushState 
      (write {}) 
      href

instance manageBlogPostAppM :: ManageBlogPost AppM where
  getBlogPosts pagination 
    = genericListRequest 
        (API.BlogPosts pagination)
        Get 
        Nothing

  getBlogPost postId 
    = genericRequest
        (API.BlogPost postId)
        Get
        Nothing

  getBlogPostBySlug slug
    = genericRequest
        (API.BlogPostBySlug $ Slug.toString slug)
        Get
        Nothing

  getBlogPostsByTagId tagId = 
    genericListRequest 
      (API.BlogPostsByTagId tagId)
      Get
      Nothing
  
  searchBlogPost query = do
    req <- mkRequest
      { endpoint: API.BlogPostSearch
      , method: Post $ Just $ encodeJson query
      , auth: Nothing
      }
    case req of
      Right json -> do
        let 
          result = decodeJson json.body :: Either JsonDecodeError (SearchResponse BlogPost)
        case result of
          Right (SearchResponse res) -> do
            let
              posts = map (\(SearchHit r) -> r.source) res.hits
            pure posts
          Left err -> do
            logMessage $ printJsonDecodeError err
            pure []
      Left err -> do
        logMessage $ printError err
        pure []

  createBlogPost post 
    = genericRequest
        API.BlogPostCreate 
        (Post $ Just $ encodeJson post)
        (Just apiAuth)

  updateBlogPost (BlogPost post) 
    = genericRequest
        (API.BlogPostUpdate post.id)
        (Post $ Just $ encodeJson $ BlogPost post)
        (Just apiAuth)

  deleteBlogPost postId = do
    req <- mkRequest
      { endpoint: API.BlogPostDelete postId
      , method: Delete
      , auth: Just apiAuth
      }
    pure unit

instance manageMediaAppM :: ManageMedia AppM where
  getImages pagination 
    = genericListRequest
        (API.Images pagination)
        Get
        Nothing

  deleteImage imageId = do
    req <- mkRequest
      { endpoint: API.ImageDelete imageId
      , method: Delete
      , auth: Just apiAuth
      }
    pure unit

  uploadImage formData = do
    req <- mkFormDataRequest
      { endpoint: API.ImageUpload
      , method: PostFormData $ Just formData
      , auth: Just apiAuth
      }
    case req of
      Right json -> do
        let img = decodeJson json.body
        case img of
          Right i -> pure $ Just i
          Left err -> do
            logMessage $ printJsonDecodeError err
            pure Nothing
      Left err -> do
        logMessage $ printError err
        pure Nothing

instance manageUserAppM :: ManageUser AppM where
  loginUser auth 
    = genericRequest 
        API.UserLogin
        Get
        (Just $ Basic $ base64encodeUserAuth auth)

instance manageTagAppM :: ManageTag AppM where
  createTag tag 
    = genericRequest
        (API.TagCreate tag)
        (Post Nothing)
        (Just apiAuth)

  getTagById tagId 
    = genericRequest
        (API.Tag tagId)
        Get
        Nothing
