module Resource.Tag where

import Prelude
import Data.Maybe           (Maybe(..))
import Halogen              (HalogenM, lift)

import Data.Tag             (Tag)

class Monad m <= ManageTag m where
  createTag :: String -> m (Maybe Tag)

instance manageTagHalogenM :: ManageTag m => ManageTag (HalogenM st act slots msg m) where
  createTag = lift <<< createTag
