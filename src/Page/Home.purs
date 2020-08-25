module Page.Home where

import Prelude
import Data.Array                           (length)
import Data.Const                           (Const)
import Data.Int                             (floor)
import Data.Maybe                           (Maybe(..))
import Data.Newtype                         (unwrap)
import Data.Symbol                          (SProxy(..))
import Data.Traversable                     (traverse)
import Effect.Class                         (class MonadEffect)
import Effect.Class.Console                 (logShow)
import Effect.Aff                           as Aff
import Effect.Aff.Class                     (class MonadAff)
import Elasticsearch.Client                 (SearchResponse(..), SearchHit(..))
import Halogen                              as H
import Halogen.HTML                         as HH
import Halogen.HTML.CSS                     as HCSS
import Halogen.HTML.Properties              as HP
import Halogen.Component.RawHTML            as RawHTML
import Halogen.Query.EventSource            as ES
import Timestamp                            (formatToDateStr)
import Web.Event.CustomEvent                as CEV
import Web.Event.Event                      as EV
import Web.HTML                             (HTMLDocument)
import Web.HTML (window)                    as Web
import Web.HTML.HTMLDocument                as HTMLDocument
import Web.HTML.HTMLElement                 as HTMLElement
import Web.HTML.Window                      as Web
import Web.DOM.Element                      as Element
import Web.DOM.ParentNode                   as PN
import Web.TouchEvent.EventTypes            as TET
import Web.TouchEvent.Touch                 as Touch
import Web.TouchEvent.TouchEvent            as TE
import Web.TouchEvent.TouchList             as TouchList
import Web.UIEvent.KeyboardEvent            (KeyboardEvent)
import Web.UIEvent.KeyboardEvent            as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.WheelEvent               as WE
import Web.UIEvent.WheelEvent.EventTypes    as WET

import Capability.Navigate                  (class Navigate, navigate)
import Component.Utils                      (OpaqueSlot)
import Component.HTML.BlogPost              (renderBlogPost)
import Component.HTML.Header                (header)
import Component.HTML.Utils                 (css, maybeElem)
import CSS.Utils                            (backgroundCover)
import Data.BlogPost                        (BlogPost(..)
                                            ,BlogPostArray)
import Data.Image                           (Image(..))
import Data.Route                           as R
import Data.Search                          (SearchQuery(..))
import Foreign.AOS                          (loadAOS)
import Foreign.Highlight                    as Highlight
import Foreign.LazyLoad                     as LZ
import Foreign.LightGallery                 (loadGallery)
import Resource.BlogPost                    (class ManageBlogPost
                                            ,searchBlogPost)
import Utils.EventTypes                     (onscroll)
import Utils.DOM                            (setHTML)


type State = 
  { blogPosts :: BlogPostArray
  , currentPage :: Int
  , scroll :: Boolean
  , lazyLoad :: Maybe LZ.LazyLoad
  }

data Action 
  = Initialize
  | HandleWheel WE.WheelEvent
  | NavigateAction R.Route

type ChildSlots = ( html :: H.Slot (Const Void) Void Unit )

initialState :: State
initialState = 
  { blogPosts: []
  , currentPage: 1
  , scroll: true
  , lazyLoad: Nothing
  }

component :: forall m
           . MonadAff m
          => MonadEffect m
          => ManageBlogPost m
          => Navigate m
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
      document <- H.liftEffect $ Web.document =<< Web.window
      -- Subscribe to eventListeners
      H.subscribe' \sid ->
        ES.eventListenerEventSource
        WET.wheel
        (HTMLDocument.toEventTarget document)
        (map HandleWheel <<< WE.fromEvent)

      -- get initial posts
      let 
        query = SearchQuery
          { queries: []
          , page: Just 1
          , perPage: Just 5 }
      posts <- searchBlogPost query
      H.modify_ _ { blogPosts = posts }
      
      -- init AOS effects
      H.liftEffect $ loadAOS

      -- init lazy load
      lazyLoad <- H.liftEffect $ LZ.createLazyLoad ".lazy"
      H.modify_ _ { lazyLoad = Just lazyLoad }

      _ <- traverse (\(BlogPost post) -> do
        let label = "element-" <> (show $ unwrap post.id)
        H.getHTMLElementRef (H.RefLabel label) >>= case _ of
          Nothing -> pure unit
          Just el -> do
            case post.htmlContent of
              Just html -> H.liftEffect $ setHTML el html
              Nothing -> pure unit) posts

      H.liftEffect $ loadGallery "lightgallery"
      H.liftAff $ Aff.delay $ Aff.Milliseconds 500.0
      -- Highlight code blocks
      H.liftEffect Highlight.highlightBlock
      pure unit

    NavigateAction route -> navigate route

    HandleWheel ev -> do
      state       <- H.get
      win         <- H.liftEffect Web.window
      scrollY     <- H.liftEffect $ Web.scrollY win
      innerHeight <- H.liftEffect $ Web.innerHeight win
      document    <- H.liftEffect $ Web.document win
      container   <- H.liftEffect $ 
                     PN.querySelector 
                     (PN.QuerySelector "body") 
                     (HTMLDocument.toParentNode document)

      case container of
        Just body -> do
          let htmlElement = HTMLElement.fromElement body
          case htmlElement of
            Just hel -> do
              scrollHeight <- H.liftEffect $ Element.scrollHeight body
              let 
                contentHeight = scrollY + innerHeight
                atBottom = contentHeight >= ((floor scrollHeight) - 50)
              case (atBottom && state.scroll) of
                true -> do
                  let
                    newCurrentPage = state.currentPage + 1
                  blogPosts <- searchBlogPost $ SearchQuery
                    { queries: []
                    , page: Just newCurrentPage
                    , perPage: Just 5 
                    }
                  case length blogPosts == 0 of
                    true -> H.modify_ _ { scroll = false }
                    false -> do
                      let allBlogPosts = state.blogPosts <> blogPosts
                      H.modify_ _ 
                        { currentPage = newCurrentPage 
                        , blogPosts = allBlogPosts
                        }
                      _ <- traverse (\(BlogPost post) -> do
                        let label = "element-" <> (show $ unwrap post.id)
                        H.getHTMLElementRef (H.RefLabel label) >>= case _ of
                          Nothing -> pure unit
                          Just el -> do
                            case post.htmlContent of
                              Just html -> H.liftEffect $ setHTML el html
                              Nothing -> pure unit) blogPosts
                      -- update lazyLoad
                      case state.lazyLoad of
                        Just lz -> H.liftEffect $ LZ.updateLazyLoad lz
                        Nothing -> pure unit

                      -- highlight blocks
                      H.liftEffect Highlight.highlightBlock

                      -- Check if number of posts is less than
                      -- perPage. If so we stop checking for more posts
                      case length blogPosts < 5 of
                        true -> H.modify_ _ { scroll = false }
                        false -> pure unit
                false -> pure unit
            Nothing  -> pure unit
        Nothing   -> pure unit
      H.liftAff $ Aff.delay $ Aff.Milliseconds 500.0

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div 
      [ css "posts-container" 
      , HP.attr (HH.AttrName "data-aos") "fade-up"
      ]
      (map (renderBlogPost NavigateAction) state.blogPosts)
