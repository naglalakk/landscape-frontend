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
import Halogen                              as H
import Halogen.HTML                         as HH
import Halogen.HTML.CSS                     as HCSS
import Halogen.HTML.Properties              as HP
import Halogen.Component.RawHTML            as RawHTML
import Halogen.Query.EventSource            as ES
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

import Component.Utils                      (OpaqueSlot)
import Component.HTML.Utils                 (css, maybeElem)
import CSS.Utils                            (backgroundCover)
import Data.BlogPost                        (BlogPost(..)
                                            ,BlogPostArray)
import Data.Image                           (Image(..))
import Foreign.LightGallery                 (loadGallery)
import Resource.BlogPost                    (class ManageBlogPost
                                            ,getBlogPosts)
import Timestamp                            (formatToDateStr)
import Utils.EventTypes                     (onscroll)
import Utils.DOM                            (setHTML)




type State = 
  { blogPosts :: BlogPostArray
  , currentPage :: Int
  , scroll :: Boolean
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
  , scroll: true
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

      H.liftEffect $ loadGallery "lightgallery"
      pure unit

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
                atBottom = contentHeight >= (floor scrollHeight)
              case (atBottom && state.scroll) of
                true -> do
                  let
                    newCurrentPage = state.currentPage + 1
                  blogPosts <- getBlogPosts { page: Just newCurrentPage
                                             , perPage: Just 5 
                                             }
                  case length blogPosts == 0 of
                    true -> H.modify_ _ { scroll = false }
                    false -> do
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

                      -- Check if number of posts is less than
                      -- perPage. If so we stop checking for more posts
                      case length blogPosts < 5 of
                        true -> H.modify_ _ { scroll = false }
                        false -> pure unit
                false -> pure unit
            Nothing  -> pure unit
        Nothing   -> pure unit

      -- TODO: Use updated debounce func in halogen-formless
      H.liftAff $ Aff.delay $ Aff.Milliseconds 500.0

    HandleScroll ev -> do
      win <- H.liftEffect Web.window
      height <- H.liftEffect $ Web.innerHeight win
      scrollY <- H.liftEffect $ Web.scrollY win
      pure unit

  socialItem :: forall i p. String -> String -> HH.HTML i p
  socialItem link icon = 
    HH.li
      [ css "social-item" ]
      [ HH.a
        [ HP.href link ]
        [ HH.i
          [ css icon ]
          []
        ]
      ]

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div
      []
      [ HH.div
        [ css "header" ]
        [ HH.div
          [ css "profile-image" 
          , HCSS.style $ backgroundCover "img/profile.gif"
          ]
          [
          ]
        , HH.h2
          [ css "title" ]
          [ HH.text "Donna" ]
        , HH.ul
          [ css "socials" ]
          [ socialItem "https://github.com/naglalakk" "fab fa-github"
          , socialItem "https://soundcloud.com/donnainternational" "fab fa-soundcloud"
          , socialItem "https://www.pinterest.com/k0ttur/" "fab fa-pinterest"
          , socialItem "https://twitter.com/naglalakk" "fab fa-twitter"
          ]
        , HH.div
          [ css "line" ]
          []
        ]
      , HH.div 
        [ css "posts-container" ]
        (map (\(BlogPost post) ->
          HH.div
            [ css $ "post cover-" <> (show post.isCover) ]
            [ case post.isCover of
              true ->
                HH.div
                  [ css "cover-image" 
                  , case post.featuredImage of
                    Just (Image image) -> HCSS.style $ backgroundCover image.src
                    Nothing -> css "no-cover"
                  ]
                  [ HH.div
                    [ css "title" ]
                    [ HH.h1
                      []
                      [ HH.text post.title ]
                    , HH.div [ css "title-line" ] []
                    , case post.showDate of
                      true -> 
                        HH.div
                          [ css "post-date" ]
                          [ HH.text $ formatToDateStr post.publishTime ]
                      false -> HH.div [] []
                    ]
                  ]
              false ->
                HH.div
                  [ css "title" ]
                  [ HH.h1
                    []
                    [ HH.text post.title ]
                  , HH.div [ css "title-line" ] []
                  , case post.showDate of
                    true -> 
                      HH.div
                        [ css "post-date" ]
                        [ HH.text $ formatToDateStr post.publishTime ]
                    false -> HH.div [] []
                  ]
            , HH.div
              [ css "post-content" 
              , HP.ref (H.RefLabel ("element-" <> (show $ unwrap post.id)))
              ]
              []
            , case length post.images of
                0 -> HH.div [] []
                _ -> 
                  HH.div
                    [ css "lightgallery" ]
                    (map (\(Image image) -> 
                      HH.a
                        [ HP.href image.src ]
                        [ HH.img
                          [ case image.thumbnail of
                            Just thumb -> HP.src thumb
                            Nothing -> HP.src image.src
                          ]
                        ]) post.images )
            ]) state.blogPosts )
      ]
