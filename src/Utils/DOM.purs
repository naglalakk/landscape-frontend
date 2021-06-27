module Utils.DOM where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Web.DOM.Document as Document
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (HTMLElement)
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

foreign import setHTML :: HTMLElement -> String -> Effect Unit

getOffsetTop :: String -> Effect (Maybe Number)
getOffsetTop id = do
  w <- HTML.window
  document <- Window.document w
  elem <- getElementById id $ Document.toNonElementParentNode (HTMLDocument.toDocument document)
  case elem of
    Just element -> do
      let
        htmlElement = HTMLElement.fromElement element
      case htmlElement of
        Just htmlElem -> do
          offsetTop <- HTMLElement.offsetTop htmlElem
          pure $ Just offsetTop 
        Nothing -> pure Nothing
    Nothing -> pure Nothing

getBoundingClientRect :: String -> Effect (Maybe HTMLElement.DOMRect)
getBoundingClientRect id = do
  w <- HTML.window
  document <- Window.document w
  elem <- getElementById id $ Document.toNonElementParentNode (HTMLDocument.toDocument document)
  case elem of
    Just element -> do
      let
        htmlElement = HTMLElement.fromElement element
      case htmlElement of
        Just htmlElem -> do
          clientRect <- HTMLElement.getBoundingClientRect htmlElem
          pure $ Just clientRect
        Nothing -> pure Nothing
    Nothing -> pure Nothing


getTop :: String -> Effect (Maybe Number)
getTop id_ = do
  w <- HTML.window
  box <- getBoundingClientRect id_
  case box of
    Just clientRect -> do
      y <- Window.scrollY w
      pure $ Just $ clientRect.top + (toNumber y)
    Nothing -> pure Nothing
