module Data.Image where

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
import Formless                 as F
import Halogen.Media.Data.Media (MediaRow)

import Config                   (apiURL)
import Timestamp                (Timestamp(..)
                                ,defaultTimestamp)



newtype ImageId = ImageId Int

derive instance newtypeImageId :: Newtype ImageId _
derive instance genericImageId :: Generic ImageId _
derive instance eqImageId :: Eq ImageId
derive instance ordImageId :: Ord ImageId

derive newtype instance encodeJsonImageId :: EncodeJson ImageId
derive newtype instance decodeJsonImageId :: DecodeJson ImageId

instance showImageId :: Show ImageId where
  show = genericShow

instance initialImageId :: F.Initial ImageId where
  initial = ImageId 0

type ImageType = (
    id :: ImageId
  , createdAt :: Timestamp
  , updatedAt :: Maybe Timestamp
)

newtype Image = Image
  { id  :: ImageId
  , createdAt :: Timestamp
  , updatedAt :: Maybe Timestamp
  | MediaRow
  }

type ImageArray = Array Image

derive instance genericImage :: Generic Image _
derive instance eqImage :: Eq Image
derive instance ordImage :: Ord Image

instance showImage :: Show Image where
  show = genericShow

instance initialImage :: F.Initial Image where
  initial = Image
    { id: (ImageId 0) 
    , src: ""
    , thumbnail: Nothing
    , createdAt: defaultTimestamp
    , updatedAt: Nothing
    }

instance decodeJsonImage :: DecodeJson Image where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    src <- obj .: "src"
    thumbnail <- obj .:? "thumbnail"
    createdAt <- obj .: "created_at"
    updatedAt <- obj .:? "updated_at"
    pure $ Image 
      { id: id
      , src: (apiURL <> "/" <> src)
      , thumbnail: map (\x -> apiURL <> "/" <> x) thumbnail
      , createdAt: createdAt
      , updatedAt: updatedAt }

instance encodeJsonImage :: EncodeJson Image where
  encodeJson (Image img) = encodeJson img.id

decodeImageArray :: Json -> Either String ImageArray
decodeImageArray json = decodeJson json >>= traverse decodeJson
