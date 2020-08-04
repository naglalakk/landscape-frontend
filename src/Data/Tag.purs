module Data.Tag where

import Prelude
import Data.Argonaut            (Json
                                ,decodeJson
                                ,encodeJson
                                ,(~>), (~>?)
                                ,(:=), (:=?)
                                ,(.:), (.:?))
import Data.Argonaut.Encode     (class EncodeJson)
import Data.Argonaut.Decode     (class DecodeJson)
import Data.Either              (Either(..))
import Data.Generic.Rep         (class Generic)
import Data.Generic.Rep.Show    (genericShow)
import Data.Maybe               (Maybe(..))
import Data.Newtype             (class Newtype)
import Data.Traversable         (traverse)
import Effect                   (Effect)
import Formless                 as F

import Timestamp                (Timestamp(..)
                                ,defaultTimestamp
                                ,nowTimestamp)

newtype TagId = TagId Int

derive instance newtypeTagId :: Newtype TagId _
derive instance genericTagId :: Generic TagId _
derive instance eqTagId      :: Eq TagId
derive instance ordTagId     :: Ord TagId

derive newtype instance encodeJsonTagId :: EncodeJson TagId
derive newtype instance decodeJsonTagId :: DecodeJson TagId

instance showTagId :: Show TagId where
  show = genericShow

newtype Tag = Tag
  { id :: TagId
  , label :: String
  , createdAt :: Timestamp
  , updatedAt :: Maybe Timestamp
  }

derive instance genericTag :: Generic Tag _
derive instance eqTag :: Eq Tag
derive instance ordTag :: Ord Tag

instance showTag :: Show Tag where
  show = genericShow

instance decodeTag :: DecodeJson Tag where
  decodeJson json = do
    obj <- decodeJson json
    id  <- obj .: "id"
    label <- obj .: "label"
    createdAt <- obj .: "created_at"
    updatedAt <- obj .:? "updated_at"
    pure $ Tag
      { id
      , label
      , createdAt
      , updatedAt
      }

type TagArray = Array Tag
