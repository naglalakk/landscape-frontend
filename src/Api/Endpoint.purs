module Api.Endpoint where

import Prelude
import Data.Generic.Rep                 (class Generic)
import Data.Generic.Rep.Show            (genericShow)
import Data.Maybe                       (Maybe(..))
import Routing.Duplex                   (RouteDuplex'
                                        ,int, optional
                                        ,path, prefix
                                        ,root, segment)
import Routing.Duplex.Generic           (sum, noArgs)
import Routing.Duplex.Generic.Syntax    ((/), (?))
import Slug                             (Slug)

import Data.BlogPost                    as BP
import Data.Image                       (ImageId)
import Utils.Route                      (blogPostId
                                        ,imageId
                                        ,slug)

type PaginationRep =
  ( page :: Maybe Int
  , perPage :: Maybe Int
  )

type Pagination = { | PaginationRep }

data Endpoint 
  = BlogPosts Pagination
  | BlogPost BP.BlogPostId
  | BlogPostBySlug String
  | BlogPostSearch
  | BlogPostCreate
  | BlogPostUpdate BP.BlogPostId
  | BlogPostDelete BP.BlogPostId
  | Images Pagination
  | ImageDelete ImageId
  | ImageUpload  
  | UserLogin

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum
  { "BlogPosts": "posts" ?
    { page: optional <<< int
    , perPage: optional <<< int
    }
  , "BlogPost" : "posts" / blogPostId
  , "BlogPostBySlug" : "posts" / slug
  , "BlogPostSearch" : "posts" / "search" / noArgs
  , "BlogPostCreate" : "posts" / noArgs
  , "BlogPostUpdate" : "posts" / blogPostId / "update"
  , "BlogPostDelete" : "posts" / blogPostId / "delete"
  , "Images" : "media" /  "images" ?
    { page: optional <<< int
    , perPage: optional <<< int
    }
  , "ImageDelete" : "media" / "images" / imageId / "delete"
  , "ImageUpload" : "media" / "images" / "upload" / noArgs
  , "UserLogin" : "users" / "authenticate" / noArgs
  }
