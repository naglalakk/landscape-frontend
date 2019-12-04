module Utils.DOM where

import Prelude
import Effect       (Effect)
import Web.HTML     (HTMLElement)

foreign import setHTML :: HTMLElement -> String -> Effect Unit
