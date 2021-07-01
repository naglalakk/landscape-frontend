module Data.Route where

import Prelude hiding ((/))

import Data.BlogPost (BlogPostId)
import Data.Exhibition (ExhibitionId(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Tag (TagId)
import Data.Token (TokenId)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Slug (Slug)
import Utils.Route (blogPostId, exhibitionId, slug, tagId, tokenId)

data Route
  = Home
  | BlogPost String
  | Exhibition ExhibitionId
  | Exhibitions
  | About
  | Login
  | Tag TagId
  -- Admin
  | AdminHome
  | AdminBlogPosts
  | AdminBlogPost BlogPostId
  | AdminExhibitions
  | AdminExhibition ExhibitionId
  | AdminTokens
  | AdminToken TokenId
  | AdminTransactions

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = sum
  { "Home": noArgs
  , "BlogPost" : "posts" / slug
  , "Exhibition" : "exhibitions" / exhibitionId
  , "Exhibitions" : "exhibitions" / noArgs
  , "About" : "about" / noArgs
  , "Login" : "login" / noArgs
  , "Tag"   : "tags" / tagId
  -- Admin
  , "AdminHome" : "admin" / noArgs
  , "AdminBlogPosts" : "admin" / "posts" / noArgs
  , "AdminBlogPost" : "admin" / "posts" / blogPostId
  , "AdminExhibitions" : "admin" / "exhibitions" / noArgs
  , "AdminExhibition" : "admin" / "exhibitions" / exhibitionId
  , "AdminTokens" : "admin" / "tokens" / noArgs
  , "AdminToken"  : "admin" / "tokens" / tokenId
  , "AdminTransactions" : "admin" / "transactions" / noArgs
  }
