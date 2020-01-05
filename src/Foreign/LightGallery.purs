module Foreign.LightGallery where

import Prelude
import Effect (Effect)

foreign import loadGallery :: String -> Effect Unit
