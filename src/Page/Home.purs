module Page.Home where

import Prelude
import Data.Array                           (length)
import Data.Const                           (Const)
import Data.Maybe                           (Maybe(..))
import Data.Newtype                         (unwrap)
import Data.Symbol                          (SProxy(..))
import Data.Traversable                     (traverse)
import Effect.Class                         (class MonadEffect)
import Effect.Class.Console                 (logShow)
import Effect.Aff                           as Aff
import Effect.Aff.Class                     (class MonadAff)
import Halogen                              as H
import Halogen.HTML                         as HH
import Halogen.HTML.Properties              as HP
import Halogen.Component.RawHTML            as RawHTML
import Halogen.Query.EventSource            as ES
import Web.Event.CustomEvent                as CEV
import Web.Event.Event                      as EV
import Web.HTML                             (HTMLDocument)
import Web.HTML (window)                    as Web
import Web.HTML.HTMLDocument                as HTMLDocument
import Web.HTML.Window                      as Web
import Web.TouchEvent.EventTypes            as TET
import Web.TouchEvent.Touch                 as Touch
import Web.TouchEvent.TouchEvent            as TE
import Web.TouchEvent.TouchList             as TouchList
import Web.UIEvent.KeyboardEvent            (KeyboardEvent)
import Web.UIEvent.KeyboardEvent            as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.WheelEvent               as WE
import Web.UIEvent.WheelEvent.EventTypes    as WET

import Component.Utils                      (OpaqueSlot)
import Component.HTML.Utils                 (css, maybeElem)
import Data.BlogPost                        (BlogPost(..)
                                            ,BlogPostArray)
import Resource.BlogPost                    (class ManageBlogPost
                                            ,getBlogPosts)
import Timestamp                            (formatToDateStr)
import Utils.EventTypes                     (onscroll)
import Utils.DOM                            (setHTML)




type State = 
  { blogPosts :: BlogPostArray
  , currentPage :: Int
  }

data Action 
  = Initialize
  | HandleWheel WE.WheelEvent
  | HandleScroll CEV.CustomEvent

type ChildSlots = ( html :: H.Slot (Const Void) Void Unit )

initialState :: State
initialState = 
  { blogPosts: []
  , currentPage: 1
  }

component :: forall m
           . MonadAff m
          => MonadEffect m
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
      document <- H.liftEffect $ Web.document =<< Web.window
      -- Subscribe to eventListeners
      H.subscribe' \sid ->
        ES.eventListenerEventSource
        onscroll
        (HTMLDocument.toEventTarget document)
        (map HandleScroll <<< CEV.fromEvent)

      H.subscribe' \sid ->
        ES.eventListenerEventSource
        WET.wheel
        (HTMLDocument.toEventTarget document)
        (map HandleWheel <<< WE.fromEvent)

      -- get initial posts
      posts <- getBlogPosts { page: Just 1
                            , perPage: Just 5 }
      H.modify_ _ { blogPosts = posts }

      _ <- traverse (\(BlogPost post) -> do
        let label = "element-" <> (show $ unwrap post.id)
        H.getHTMLElementRef (H.RefLabel label) >>= case _ of
          Nothing -> pure unit
          Just el -> do
            case post.htmlContent of
              Just html -> H.liftEffect $ setHTML el html
              Nothing -> pure unit) posts
      pure unit

    HandleWheel ev -> do
      win <- H.liftEffect Web.window
      height <- H.liftEffect $ Web.innerHeight win
      scrollY <- H.liftEffect $ Web.scrollY win
      case ((height - 250) - scrollY <= 0) of
        true -> do
          state <- H.get
          let
            newCurrentPage = state.currentPage + 1
          blogPosts <- getBlogPosts { page: Just newCurrentPage
                                     , perPage: Just 5 
                                     }
          case length blogPosts of
            0 -> pure unit
            _ -> do
              let allBlogPosts = state.blogPosts <> blogPosts
              H.modify_ _ { currentPage = newCurrentPage 
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
              pure unit
        false -> pure unit
      H.liftAff $ Aff.delay $ Aff.Milliseconds 500.0
      logShow height
      logShow scrollY
      pure unit

    HandleScroll ev -> do
      win <- H.liftEffect Web.window
      height <- H.liftEffect $ Web.innerHeight win
      scrollY <- H.liftEffect $ Web.scrollY win
      logShow height
      logShow scrollY
      pure unit


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
            [ css "post-content" 
            , HP.ref (H.RefLabel ("element-" <> (show $ unwrap post.id)))
            ]
            []
          ]) state.blogPosts )
