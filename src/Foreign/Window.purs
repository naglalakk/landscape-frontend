module Foreign.Window where

import Prelude
import Effect               (Effect)
import Web.HTML.Window      (Window)

foreign import scrollTo :: Int -> Int -> Window -> Effect Unit
