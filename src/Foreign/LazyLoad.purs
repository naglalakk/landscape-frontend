module Foreign.LazyLoad where

import Prelude
import Effect (Effect)

foreign import lazyLoad :: String -> Effect Unit
foreign import lazyLoadUpdate :: Effect Unit
