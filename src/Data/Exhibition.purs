module Data.Exhibition where

import Prelude

import Data.Argonaut (decodeJson, encodeJson, jsonEmptyObject, (:=), (~>), (.:), (.:?))
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Image (Image(..))
import Data.Item (ItemId)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Formless as F
import Timestamp (Timestamp)

newtype ExhibitionId = ExhibitionId Int

derive instance newtypeExhibitionId :: Newtype ExhibitionId _
derive instance genericExhibitionId :: Generic ExhibitionId _
derive instance eqExhibitionId :: Eq ExhibitionId
derive instance ordExhibitionId :: Ord ExhibitionId

instance initialExhibitionId :: F.Initial ExhibitionId where
  initial = ExhibitionId 0

derive newtype instance encodeJsonExhibitionId :: EncodeJson ExhibitionId
derive newtype instance decodeJsonExhibitionId :: DecodeJson ExhibitionId

instance showExhibitionId :: Show ExhibitionId where
  show = genericShow

newtype Exhibition = Exhibition
  { id :: ExhibitionId
  , title :: String
  , featuredImage :: Maybe Image
  , introduction :: Maybe String
  , items :: Array ItemId
  , startDate :: Timestamp
  , endDate :: Timestamp
  , createdAt :: Timestamp
  , updatedAt :: Maybe Timestamp
  }

derive instance newtypeExhibition :: Newtype Exhibition _
derive instance genericExhibition :: Generic Exhibition _
derive instance eqExhibition :: Eq Exhibition

instance encodeJsonExhibition :: EncodeJson Exhibition where
  encodeJson (Exhibition exhibition) 
    =  "title" := exhibition.title
    ~> "featuredImage" := map (\(Image img) -> img.id) exhibition.featuredImage
    ~> "introduction" := exhibition.introduction
    ~> "items" := exhibition.items
    ~> "startDate" := exhibition.startDate
    ~> "endDate" := exhibition.endDate
    ~> "createdAt" := exhibition.createdAt
    ~> "updatedAt" := exhibition.updatedAt

derive newtype instance decodeJsonExhibition :: DecodeJson Exhibition
