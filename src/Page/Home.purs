module Page.Home where

import Prelude
import Data.Const                   (Const)
import Data.Maybe                   (Maybe(..))
import Data.Symbol                  (SProxy(..))
import Halogen                      as H
import Halogen.HTML                 as HH

import Component.Utils              (OpaqueSlot)
import Component.HTML.Utils         (css)
import Data.BlogPost                (BlogPost(..)
                                    ,BlogPostArray)
import Resource.BlogPost            (class ManageBlogPost
                                    ,getBlogPosts)
import Timestamp                    (formatToDateStr)

type State = 
  { blogPosts :: BlogPostArray
  }

data Action 
  = Initialize

type ChildSlots = ()

initialState :: State
initialState = 
  { blogPosts: []
  }

component :: forall m
           . ManageBlogPost m
          => H.Component HH.HTML (Const Void) Unit Void m
component =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
    }
  where
  handleAction = case _ of
    Initialize -> do
      posts <- getBlogPosts { page: Just 1
                            , perPage: Just 25 }
      H.modify_ _ { blogPosts = posts }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div 
      [ css "posts-container" ]
      (map (\(BlogPost post) ->
        HH.div
          [ css "post" ]
          [ HH.h1
            []
            [ HH.text post.title ]
          , HH.div
            [ css "post-date" ]
            [ HH.text $ formatToDateStr post.publishTime ]
          , HH.div
            [ css "post-content" ]
            [ HH.text post.content ]
          ]
        ) state.blogPosts )
