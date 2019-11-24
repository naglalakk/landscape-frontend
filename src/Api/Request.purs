module Api.Request where

import Prelude

import Affjax                       (Request, request)
import Affjax.RequestBody           as RB
import Affjax.ResponseFormat        as RF
import Affjax.RequestHeader         (RequestHeader(..))
import Control.Monad.Reader.Class   (class MonadAsk, ask, asks)
import Data.Argonaut.Core           (Json)
import Data.Either                  (Either(..), hush)
import Data.Maybe                   (Maybe(..))
import Data.HTTP.Method             (Method(..))
import Data.Tuple                   (Tuple(..))
import Effect.Aff.Class             (class MonadAff, liftAff)
import Routing.Duplex               (print)
import Web.XHR.FormData             (FormData)

import Api.Endpoint                 (Endpoint)
import Data.URL                     (BaseURL(..))
import Api.Endpoint                 (Endpoint(..)
                                    ,endpointCodec)

newtype Token = Token String

derive instance eqToken  :: Eq Token
derive instance ordToken :: Ord Token

instance showToken :: Show Token where
  show (Token _) = "Token {- token -}"

data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

data FormDataRequestMethod
  = PostFormData (Maybe FormData)

type RequestOptions =
  { endpoint :: Endpoint
  , method   :: RequestMethod
  }

type FormDataOptions =
  { endpoint :: Endpoint
  , method   :: FormDataRequestMethod
  }

defaultRequest :: BaseURL        ->
                  Maybe Token    ->
                  RequestOptions ->
                  Request Json
defaultRequest (BaseURL baseURL) auth { endpoint, method } =
  { method: Left method
  , url: baseURL <> print endpointCodec endpoint
  , headers: case auth of
      Nothing        -> []
      Just (Token t) -> [ RequestHeader "Authorization" $ "Token " <> t ]
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }
  where
  Tuple method body = case method of
    Get    -> Tuple GET Nothing
    Post b -> Tuple POST b
    Put  b -> Tuple PUT b
    Delete -> Tuple DELETE Nothing

formDataRequest :: BaseURL         ->
                   Maybe Token     ->
                   FormDataOptions ->
                   Request Json
formDataRequest (BaseURL baseURL) auth { endpoint, method } =
  { method: Left method
  , url: baseURL <> print endpointCodec endpoint
  , headers: case auth of
      Nothing        -> []
      Just (Token t) -> [ RequestHeader "Authorization" $ "Token " <> t ]
  , content: RB.formData <$> body
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }
  where
  Tuple method body = case method of
    PostFormData b -> Tuple POST b

mkRequest :: forall m r
           . MonadAff m
          => MonadAsk { apiURL :: BaseURL | r } m
          => RequestOptions
          -> m (Maybe Json)
mkRequest opts = do
  { apiURL } <- ask
  response <- liftAff $ request $ defaultRequest apiURL Nothing opts
  pure $ hush response.body

mkFormDataRequest :: forall m r
                   . MonadAff m
                  => MonadAsk { apiURL :: BaseURL | r } m
                  => FormDataOptions
                  -> m (Maybe Json)
mkFormDataRequest opts = do
  { apiURL } <- ask
  response <- liftAff $ request $ formDataRequest apiURL Nothing opts
  pure $ hush response.body
