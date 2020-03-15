module Page.BlogPost where

-- Page for a single BlogPost

import Prelude
import Data.Const                   (Const)
import Data.Maybe                   (Maybe(..))
import Data.Newtype                 (unwrap)
import Effect.Aff.Class             (class MonadAff)
import Effect.Class                 (class MonadEffect)
import Halogen                      as H
import Halogen.HTML                 as HH
import Slug                         as Slug

import Component.HTML.BlogPost      (renderBlogPost)
import Component.HTML.Header        (header)
import Component.HTML.Utils         (css)
import Data.BlogPost                (BlogPost(..))
import Resource.BlogPost            (class ManageBlogPost
                                    ,getBlogPostBySlug)
import Utils.DOM                    (setHTML)

data Action
  = Initialize
  | LoadBlogPost (Maybe Slug.Slug)

type Query = Const Void

type Input = 
  { slug :: Maybe Slug.Slug
  }

type State = 
  { slug     :: Maybe Slug.Slug
  , blogPost :: Maybe BlogPost
  }

type ChildSlots = ()

initialState :: Input -> State
initialState inp = 
  { slug: inp.slug
  , blogPost: Nothing
  }

component :: forall m
           . MonadAff m
          => MonadEffect m
          => ManageBlogPost m
          => H.Component HH.HTML Query Input Void m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
    }
  where
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      void $ H.fork $ handleAction $ LoadBlogPost state.slug

    LoadBlogPost slug -> case slug of
      Just s -> do
        blogPost <- getBlogPostBySlug s
        H.modify_ _ { blogPost = blogPost }
        case blogPost of
          Just (BlogPost post) -> do
            let label = "element-" <> (show $ unwrap post.id)
            H.getHTMLElementRef (H.RefLabel label) >>= case _ of
              Nothing -> pure unit
              Just el -> do
                case post.htmlContent of
                  Just html -> H.liftEffect $ setHTML el html
                  Nothing -> pure unit
          Nothing -> pure unit
      Nothing -> pure unit


  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = 
    HH.div 
      [] 
      [ header
      , case state.blogPost of 
        Just post -> 
          HH.div
            [ css "posts-container" ]
            [ renderBlogPost post   ]
        Nothing -> 
          HH.div 
            [] 
            [ HH.h1
              [ css "space-top text-center" ]
              [ HH.text "Not found" ]
            , HH.p 
              [ css "text-center" ]
              [ HH.text "Whoops this blog post was not found. Verify that the url you entered is correct." ]
            ]
      ]
