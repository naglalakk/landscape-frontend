module Resource.BlogPost where

import Prelude
import Api.Endpoint         (Pagination)
import Data.Maybe           (Maybe(..))
import Elasticsearch.Client (SearchResponse(..))
import Halogen              (HalogenM, lift)
import Slug                 (Slug)
import Web.XHR.FormData     as FD

import Data.BlogPost        (BlogPostId
                            ,BlogPost
                            ,BlogPostArray)
import Data.Search          (SearchQuery)

class Monad m <= ManageBlogPost m where
  getBlogPosts      :: Pagination   -> m BlogPostArray
  getBlogPost       :: BlogPostId   -> m (Maybe BlogPost)
  getBlogPostBySlug :: Slug         -> m (Maybe BlogPost)
  searchBlogPost    :: SearchQuery  -> m BlogPostArray
  createBlogPost    :: BlogPost     -> m (Maybe BlogPost)
  updateBlogPost    :: BlogPost     -> m (Maybe BlogPost)
  deleteBlogPost    :: BlogPostId   -> m Unit

instance manageBlogPostHalogenM :: ManageBlogPost m => ManageBlogPost (HalogenM st act slots msg m) where
  getBlogPosts      = lift <<< getBlogPosts
  getBlogPost       = lift <<< getBlogPost
  getBlogPostBySlug = lift <<< getBlogPostBySlug
  searchBlogPost    = lift <<< searchBlogPost
  createBlogPost    = lift <<< createBlogPost
  updateBlogPost    = lift <<< updateBlogPost
  deleteBlogPost    = lift <<< deleteBlogPost
