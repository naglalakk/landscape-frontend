module Component.HTML.BlogPost where

import Prelude
import Data.Array                           (length)
import Data.Maybe                           (Maybe(..))
import Data.Newtype                         (unwrap)
import Halogen                              as H
import Halogen.HTML                         as HH
import Halogen.HTML.CSS                     as HCSS
import Halogen.HTML.Properties              as HP
import Timestamp                            (formatToDateStr)

import Component.HTML.Utils                 (css)
import CSS.Utils                            (backgroundCover)
import Data.BlogPost                        (BlogPost(..)
                                            ,BlogPostArray)
import Data.Image                           (Image(..))
import Data.Tag                             (Tag(..))

renderBlogPost :: forall i p. BlogPost -> HH.HTML i p 
renderBlogPost (BlogPost post) = 
  HH.div
    [ css $ "post cover-" <> (show post.isCover) 
    ]
    [ case post.isCover of
      true ->
        HH.div
          [ css "cover-image lazy" 
          , case post.featuredImage of
            Just (Image image) -> 
              HH.attr (HH.AttrName "data-bg") image.src
              -- HCSS.style $ backgroundCover image.src
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
                  [ css "lazy"
                  , case image.thumbnail of
                    Just thumb -> 
                      HH.attr (HH.AttrName "data-src") thumb
                    Nothing -> 
                      HH.attr (HH.AttrName "data-src") image.src
                  ]
                ]) post.images )
    , case length post.tags of
        0 -> HH.div [] []
        _ -> 
          HH.div
            [ css "post-tags" ]
            [ HH.span
              []
              [ HH.text "Tags: " ]
            , HH.ul
                [ css "tags" ]
                ( map (\(Tag tag) ->
                  HH.li
                    []
                    [ HH.a
                      [ css "tag" ]
                      [ HH.text tag.label ]
                    ]) post.tags )
            ]
    ]
