module Api.Request where

import Prelude

import Affjax                       (Request, request)
import Affjax.RequestBody           as RB
import Affjax.ResponseFormat        as RF
import Affjax.RequestHeader         (RequestHeader(..))
import Control.Monad.Reader.Class   (class MonadAsk, ask, asks)
import Data.Argonaut.Core           (Json)
import Data.Argonaut.Encode         (class EncodeJson)
import Data.Argonaut.Decode         (class DecodeJson)
import Data.Either                  (Either(..), hush)
import Data.Maybe                   (Maybe(..))
import Data.HTTP.Method             (Method(..))
import Data.Tuple                   (Tuple(..))
import Effect.Aff.Class             (class MonadAff, liftAff)
import Data.Generic.Rep             (class Generic)
import Data.Generic.Rep.Show        (genericShow)
import Data.Newtype                 (class Newtype)
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



newtype Basic = Basic String

derive instance newtypeBasic :: Newtype Basic _
derive instance genericBasic :: Generic Basic _
derive instance eqBasic :: Eq Basic
derive instance ordBasic :: Ord Basic

derive newtype instance encodeJsonBasic :: EncodeJson Basic
derive newtype instance decodeJsonBasic :: DecodeJson Basic

instance showBasic :: Show Basic where
  show = genericShow


data Authentication = TokenAuth Token | BasicAuth Basic

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
  , auth     :: Maybe Authentication
  }

type FormDataOptions =
  { endpoint :: Endpoint
  , method   :: FormDataRequestMethod
  , auth     :: Maybe Authentication
  }

defaultRequest :: BaseURL        ->
                  RequestOptions ->
                  Request Json
defaultRequest (BaseURL baseURL) { endpoint, method, auth} =
  { method: Left method
  , url: baseURL <> print endpointCodec endpoint
  , headers: case auth of
      Just (BasicAuth (Basic key)) -> [ RequestHeader "Authorization" $ "Basic " <> key ]
      Just _ -> []
      Nothing        -> []
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
                   FormDataOptions ->
                   Request Json
formDataRequest (BaseURL baseURL) { endpoint, method, auth} =
  { method: Left method
  , url: baseURL <> print endpointCodec endpoint
  , headers: case auth of
      Just (BasicAuth (Basic key)) -> [ RequestHeader "Authorization" $ "Basic " <> key ]
      Just      _          -> []
      Nothing              -> []
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
  response <- liftAff $ request $ defaultRequest apiURL opts
  pure $ hush response.body

mkFormDataRequest :: forall m r
                   . MonadAff m
                  => MonadAsk { apiURL :: BaseURL | r } m
                  => FormDataOptions
                  -> m (Maybe Json)
mkFormDataRequest opts = do
  { apiURL } <- ask
  response <- liftAff $ request $ formDataRequest apiURL opts
  pure $ hush response.body
