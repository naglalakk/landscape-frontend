module Data.Item where

import Prelude

import Data.Argonaut (decodeJson, encodeJson, jsonEmptyObject, (:=), (~>), (.:), (.:?))
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Image (Image)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Token (Token(..))
import Formless as F
import Timestamp (Timestamp)

newtype ItemId = ItemId Int

derive instance newtypeItemId :: Newtype ItemId _
derive instance genericItemId :: Generic ItemId _
derive instance eqItemId :: Eq ItemId

instance initialItemId :: F.Initial ItemId where
  initial = ItemId 0

derive newtype instance encodeJsonItemId :: EncodeJson ItemId
derive newtype instance decodeJsonItemId :: DecodeJson ItemId

instance showItemId :: Show ItemId where
  show = genericShow

newtype Item = Item
  { id :: ItemId
  , title :: String
  , image :: Maybe Image
  , token :: Maybe Token
  , createdAt :: Timestamp
  , updatedAt :: Maybe Timestamp
  }

derive instance newtypeItem :: Newtype Item _
derive instance genericItem :: Generic Item _
derive instance eqItem :: Eq Item

instance encodeJsonItem :: EncodeJson Item where
  encodeJson (Item item) 
    = "title" := item.title
    ~> "image" := item.image
    ~> "token" := map (\(Token token) -> token.id) item.token
    ~> "createdAt" := item.createdAt
    ~> "updatedAt" := item.updatedAt

derive newtype instance decodeJsonItem :: DecodeJson Item
