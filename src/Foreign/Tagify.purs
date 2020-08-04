module Foreign.Tagify where

import Prelude
import Effect (Effect)

foreign import data Tagify :: Type

foreign import createTagify :: String -> Effect Tagify

foreign import addTags :: Tagify -> Array String -> Effect Unit

foreign import getTags :: Tagify -> Effect (Array String)
