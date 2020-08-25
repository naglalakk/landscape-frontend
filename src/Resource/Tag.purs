module Resource.Tag where

import Prelude
import Data.Maybe           (Maybe(..))
import Halogen              (HalogenM, lift)

import Data.Tag             (Tag, TagId)

class Monad m <= ManageTag m where
  createTag  :: String -> m (Maybe Tag)
  getTagById :: TagId -> m (Maybe Tag)

instance manageTagHalogenM :: ManageTag m => ManageTag (HalogenM st act slots msg m) where
  createTag = lift <<< createTag
  getTagById = lift <<< getTagById
