module Api.Endpoint where

import Prelude

import Data.BlogPost as BP
import Data.Exhibition (ExhibitionId)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Image (ImageId)
import Data.Item (ItemId)
import Data.Maybe (Maybe(..))
import Data.Tag (TagId)
import Data.Token (TokenId, TokenTransactionId)
import Routing.Duplex (RouteDuplex', int, optional, path, prefix, root, segment, string)
import Routing.Duplex.Generic (sum, noArgs)
import Routing.Duplex.Generic.Syntax ((/), (?))
import Slug (Slug)
import Utils.Route (blogPostId, exhibitionId, imageId, itemId, slug, tag, tagId, tokenId)

type PaginationRep =
  ( page :: Maybe Int
  , perPage :: Maybe Int
  )

type Pagination = { | PaginationRep }

type Status = 
  { status :: Maybe String
  }

data Endpoint 
  = BlogPosts Pagination
  | BlogPost BP.BlogPostId
  | BlogPostBySlug String
  | BlogPostsByTagId TagId
  | BlogPostSearch
  | BlogPostCreate
  | BlogPostUpdate BP.BlogPostId
  | BlogPostDelete BP.BlogPostId
  | Exhibitions
  | Exhibition ExhibitionId
  | ExhibitionItems ExhibitionId
  | ExhibitionUpdate ExhibitionId
  | ExhibitionDelete ExhibitionId
  | Images Pagination
  | ImageDelete ImageId
  | ImageUpload  
  | Items
  | Item ItemId
  | ItemUpdate ItemId
  | ItemDelete ItemId
  | UserLogin
  | TagCreate String
  | Tag TagId
  | Tokens
  | Token TokenId
  | TokenAmount TokenId
  | TokenAmountByHash String
  | TokenUpdate TokenId
  | TokenDelete TokenId
  | TokenRequest TokenId
  | TokenUpdateTxStatus String String
  | TokenTransactions Status
  | TokenTransaction String
  | TokenTransactionUpdate

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
  , "BlogPostsByTagId"  : "posts" / "tags" / tagId
  , "BlogPostSearch" : "posts" / "search" / noArgs
  , "BlogPostCreate" : "posts" / noArgs
  , "BlogPostUpdate" : "posts" / blogPostId / "update"
  , "BlogPostDelete" : "posts" / blogPostId / "delete"
  , "Exhibitions" : "exhibitions" / noArgs
  , "Exhibition" : "exhibitions" / exhibitionId 
  , "ExhibitionItems" : "exhibitions" / exhibitionId / "items" 
  , "ExhibitionUpdate" : "exhibitions" / exhibitionId / "update" 
  , "ExhibitionDelete" : "exhibitions"  / exhibitionId / "delete" 
  , "Images" : "media" /  "images" ?
    { page: optional <<< int
    , perPage: optional <<< int
    }
  , "ImageDelete" : "media" / "images" / imageId / "delete"
  , "ImageUpload" : "media" / "images" / "upload" / noArgs
  , "Items" : "items" / noArgs
  , "Item" : "items" / itemId
  , "ItemUpdate" : "items" / itemId / "update"
  , "ItemDelete" : "items" / itemId / "delete"
  , "UserLogin" : "users" / "authenticate" / noArgs
  , "Tag" : "tags" / tagId
  , "TagCreate" : "tags" / tag
  , "Tokens" : "tokens" / noArgs
  , "Token" : "tokens" / tokenId
  , "TokenAmount" : "tokens" / tokenId / "amount"
  , "TokenAmountByHash": "tokens" / "amount" / (string segment)
  , "TokenUpdate" : "tokens" / tokenId / "update"
  , "TokenDelete" : "tokens" / tokenId / "delete"
  , "TokenRequest" : "tokens" / tokenId / "request"
  , "TokenUpdateTxStatus" : "tokens" / "transactions" / "update" / "status" / (string segment) / (string segment)
  , "TokenTransactions" : "tokens" / "transactions" ? 
      { status: optional <<< string
      }
  , "TokenTransaction" : "tokens" / "transactions" / string segment
  , "TokenTransactionUpdate" : "tokens" / "transactions" / "update" / noArgs
  }
