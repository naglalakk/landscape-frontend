module Foreign.LazyLoad where

import Prelude
import Effect (Effect)

foreign import data LazyLoad :: Type

foreign import createLazyLoad :: String -> Effect LazyLoad
foreign import updateLazyLoad :: LazyLoad -> Effect Unit
