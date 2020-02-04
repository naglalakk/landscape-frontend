module Foreign.Flatpickr where

import Prelude
import Effect   (Effect(..))

foreign import loadFlatpickr :: String -> Effect Unit
