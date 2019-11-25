module Page.Home where

import Prelude
import Data.Const                   (Const)
import Data.Maybe                   (Maybe(..))
import Data.Symbol                  (SProxy(..))
import Effect.Class                 (class MonadEffect)
import Halogen                      as H
import Halogen.HTML                 as HH
import Halogen.Component.RawHTML    as RawHTML

import Component.Utils              (OpaqueSlot)
import Component.HTML.Utils         (css, maybeElem)
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

type ChildSlots = ( html :: H.Slot (Const Void) Void Unit )

initialState :: State
initialState = 
  { blogPosts: []
  }

component :: forall m
           . MonadEffect m
          => ManageBlogPost m
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
          , maybeElem post.htmlContent \htmlContent ->
            HH.slot (SProxy :: _ "html") unit RawHTML.component { html: htmlContent, elRef: "blogDiv" } absurd
          ]
        ) state.blogPosts )
