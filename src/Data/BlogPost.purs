module Data.BlogPost where

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
import Halogen.Media.Data.Media (MediaRow)
import Slug                     as Slug

import Data.Image               (Image(..), ImageArray)
import Data.Tag                 (Tag(..), TagArray)
import Timestamp                (Timestamp(..)
                                ,defaultTimestamp
                                ,nowTimestamp)

newtype BlogPostId = BlogPostId Int

derive instance newtypeBlogPostId :: Newtype BlogPostId _
derive instance genericBlogPostId :: Generic BlogPostId _
derive instance eqBlogPostId :: Eq BlogPostId
derive instance ordBlogPostId :: Ord BlogPostId

derive newtype instance encodeJsonBlogPostId :: EncodeJson BlogPostId
derive newtype instance decodeJsonBlogPostId :: DecodeJson BlogPostId

instance showBlogPostId :: Show BlogPostId where
  show = genericShow

instance initialBlogPostId :: F.Initial BlogPostId where
  initial = BlogPostId 0

newtype BlogPost = BlogPost
  { id            :: BlogPostId
  , title         :: String
  , slug          :: Maybe Slug.Slug
  , content       :: String
  , htmlContent   :: Maybe String
  , featuredImage :: Maybe Image
  , images        :: ImageArray
  , tags          :: TagArray
  , published     :: Boolean
  , publishTime   :: Timestamp
  , showDate      :: Boolean
  , isCover       :: Boolean
  , createdAt     :: Timestamp
  , updatedAt     :: Maybe Timestamp
  }

type BlogPostArray = Array BlogPost

derive instance genericBlogPost :: Generic BlogPost _
derive instance eqBlogPost :: Eq BlogPost
derive instance ordBlogPost :: Ord BlogPost

instance showBlogPost :: Show BlogPost where
  show = genericShow

instance decodeJsonBlogPost :: DecodeJson BlogPost where
  decodeJson json = do
    obj           <- decodeJson json
    id            <- obj .:  "id"
    title         <- obj .:  "title"
    slugStr       <- obj .:  "slug"
    let slug      = Slug.generate slugStr
    content       <- obj .:  "content"
    htmlContent   <- obj .:? "htmlContent"
    featuredImage <- obj .:? "featured_image"
    images        <- obj .:  "images"
    tags          <- obj .:  "tags"
    published     <- obj .:  "published"
    publishTime   <- obj .:  "publish_time"
    showDate      <- obj .:  "show_date"
    isCover       <- obj .:  "is_cover"
    createdAt     <- obj .:  "created_at"
    updatedAt     <- obj .:? "updated_at"
    pure $ BlogPost
      { id
      , title
      , slug
      , content
      , htmlContent
      , featuredImage
      , images
      , tags
      , published
      , publishTime
      , showDate
      , isCover
      , createdAt
      , updatedAt
      }

instance encodeJsonBlogPost :: EncodeJson BlogPost where
  encodeJson (BlogPost blogPost) 
    =  "title"          := blogPost.title
    ~> "slug"           := blogPost.slug
    ~> "content"        := blogPost.content
    ~> "htmlContent"    := blogPost.htmlContent
    ~> "featured_image" := blogPost.featuredImage
    ~> "images"         := blogPost.images
    ~> "tags"           := map (\(Tag tag) -> tag.id) blogPost.tags
    ~> "published"      := blogPost.published
    ~> "publish_time"   := blogPost.publishTime
    ~> "show_date"      := blogPost.showDate
    ~> "is_cover"       := blogPost.isCover
    ~> "created_at"     := blogPost.createdAt
    ~> "updated_at"     := blogPost.updatedAt

decodeBlogPostArray :: Json -> Either String BlogPostArray
decodeBlogPostArray json = decodeJson json >>= traverse decodeJson

defaultBlogPost :: Effect BlogPost
defaultBlogPost = do
  now <- nowTimestamp
  pure $ BlogPost
    { id: (BlogPostId 0)
    , title: ""
    , slug: Nothing
    , content: "{ 'ops': [] }"
    , htmlContent: Nothing
    , featuredImage: Nothing
    , images: []
    , tags: []
    , published: false
    , publishTime: now
    , showDate: true
    , isCover: false
    , createdAt: now
    , updatedAt: Nothing
    }
