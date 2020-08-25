module Data.Route where

import Prelude hiding                   ((/))
import Data.Generic.Rep                 (class Generic)
import Data.Generic.Rep.Show            (genericShow)
import Routing.Duplex                   (RouteDuplex', root)
import Routing.Duplex.Generic           (noArgs, sum)
import Routing.Duplex.Generic.Syntax    ((/))
import Slug                             (Slug)

import Data.BlogPost                    (BlogPostId)
import Data.Tag                         (TagId)
import Utils.Route                      (blogPostId
                                        ,slug
                                        ,tagId)

data Route
  = Home
  | BlogPost String
  | Login
  | Tag TagId
  -- Admin
  | AdminHome
  | AdminBlogPosts
  | AdminBlogPost BlogPostId

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = sum
  { "Home": noArgs
  , "BlogPost" : "posts" / slug
  , "Login" : "login" / noArgs
  , "Tag"   : "tags" / tagId
  -- Admin
  , "AdminHome" : "admin" / noArgs
  , "AdminBlogPosts" : "admin" / "posts" / noArgs
  , "AdminBlogPost" : "admin" / "posts" / blogPostId
  }
