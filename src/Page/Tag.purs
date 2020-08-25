module Page.Tag where

import Prelude
import Data.Const               (Const)
import Data.Maybe               (Maybe(..))
import Effect.Class             (class MonadEffect)
import Halogen                  as H
import Halogen.HTML             as HH
import Halogen.HTML.Events      as HE
import Halogen.HTML.Properties  as HP

import Capability.Navigate      (class Navigate, navigate)
import Component.HTML.BlogPost  (renderBlogPostPreview)
import Component.HTML.Utils     (css, maybeElem)
import Data.BlogPost            (BlogPostArray)
import Data.Route               as R
import Data.Tag                 (Tag(..), TagId)
import Resource.BlogPost        (class ManageBlogPost
                                ,getBlogPostsByTagId)
import Resource.Tag             (class ManageTag, getTagById)

type Input =
  { tagId :: TagId
  }

type State = 
  { tagId       :: TagId
  , tag         :: Maybe Tag
  , blogPosts   :: BlogPostArray
  }

type Query = Const Void
type Output = Void
type ChildSlots = ()

data Action 
  = Initialize
  | NavigateAction R.Route

component :: forall m
           . MonadEffect m
          => ManageBlogPost m
          => ManageTag m
          => Navigate m
          => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction 
      , initialize = Just Initialize
      }
    }
  where
  initialState :: Input -> State
  initialState inp = 
    { tagId: inp.tagId
    , blogPosts: []
    , tag: Nothing
    }
  
  handleAction :: Action 
               -> H.HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      tag <- getTagById state.tagId
      blogPosts <- getBlogPostsByTagId state.tagId
      H.modify_ _ { blogPosts = blogPosts 
                  , tag = tag
                  }

    NavigateAction route -> navigate route

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = 
    HH.div
      [ css "tags-wrapper" ]
      ([ HH.h1
        []
        [ maybeElem state.tag \(Tag tag) ->
          HH.text tag.label
        ]
      ] <> (state.blogPosts <#> (renderBlogPostPreview NavigateAction)))
