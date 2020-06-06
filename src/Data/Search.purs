module Data.Search where

import Prelude
import Data.Argonaut.Encode     (class EncodeJson)
import Data.Argonaut.Decode     (class DecodeJson)
import Data.Maybe               (Maybe(..))
import Data.Newtype             (class Newtype)

type FieldQuery = Array String

newtype SearchQuery = SearchQuery
  { queries :: Array FieldQuery
  , page    :: Maybe Int
  , perPage :: Maybe Int
  }

derive instance newtypeSearchQuery :: Newtype SearchQuery _

derive newtype instance decodeJsonSearchQuery :: DecodeJson SearchQuery
derive newtype instance encodeJsonSearchQuery :: EncodeJson SearchQuery
