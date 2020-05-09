module Page.BlogPost where

-- Page for a single BlogPost

import Prelude
import Data.Const                   (Const)
import Data.Maybe                   (Maybe(..), fromMaybe)
import Data.Newtype                 (unwrap)
import Effect.Aff.Class             (class MonadAff)
import Effect.Class                 (class MonadEffect)
import Foreign.LazyLoad             as LZ
import Halogen                      as H
import Halogen.HTML                 as HH
import Slug                         as Slug
import Web.HTML                     (window)
import Web.DOM.Document             as DOC
import Web.DOM.Element              as DOM
import Web.DOM.HTMLCollection       as HTMLCollection
import Web.HTML.HTMLDocument        as HTMLDoc
import Web.HTML.Window              as Window

import Component.HTML.BlogPost      (renderBlogPost)
import Component.HTML.Header        (header)
import Component.HTML.Utils         (css, safeHref)
import Data.BlogPost                (BlogPost(..))
import Data.Image                   (Image(..))
import Data.Route                   as R
import Resource.BlogPost            (class ManageBlogPost
                                    ,getBlogPostBySlug)
import Utils.DOM                    (setHTML)
import Utils.Site                   (siteURL)

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
        H.liftEffect $ LZ.lazyLoad ".lazy"
        case blogPost of
          Just (BlogPost post) -> do
            -- Update og meta info
            -- TODO: Maybe put this into a util function?
            document <- H.liftEffect $ window >>= Window.document
            metas <- H.liftEffect $ DOC.getElementsByTagName "meta" $ HTMLDoc.toDocument document
            titleMeta <- H.liftEffect $ HTMLCollection.namedItem "og_title" metas
            imageMeta <- H.liftEffect $ HTMLCollection.namedItem "og_image" metas
            urlMeta <- H.liftEffect $ HTMLCollection.namedItem "og_url" metas
            case titleMeta of
              Just tMeta ->
                H.liftEffect $ DOM.setAttribute "content" post.title tMeta
              Nothing -> pure unit
            case imageMeta of
              Just iMeta ->
                case post.featuredImage of
                  Just (Image img) -> case img.thumbnail of
                    Just thumb -> H.liftEffect $ DOM.setAttribute "content" thumb iMeta
                    Nothing -> H.liftEffect $ DOM.setAttribute "content" img.src iMeta
                  Nothing -> pure unit
              Nothing -> pure unit
            case urlMeta of
              Just uMeta -> case post.slug of
                Just slug -> H.liftEffect $ DOM.setAttribute "content" (siteURL <> "/#/posts/" <> Slug.toString slug) uMeta
                Nothing -> pure unit
              Nothing -> pure unit
            let label = "element-" <> (show $ unwrap post.id)
            H.getHTMLElementRef (H.RefLabel label) >>= case _ of
              Nothing -> pure unit
              Just el -> do
                case post.htmlContent of
                  Just html -> do
                    H.liftEffect $ setHTML el html
                  Nothing -> pure unit
          Nothing -> pure unit
      Nothing -> pure unit


  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = 
    HH.div 
      [] 
      [ case state.blogPost of 
        Just post -> 
          HH.div
            [ css "posts-container post-single" ]
            [ renderBlogPost post   
            , HH.a
              [ safeHref R.Home
              , css "navigation-back text-center" 
              ]
              [ HH.text "Back to site" ]
            ]
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
