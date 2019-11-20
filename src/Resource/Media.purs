module Resource.Media where

import Prelude
import Api.Endpoint       (Pagination)
import Data.Maybe         (Maybe(..))
import Halogen            (HalogenM, lift)
import Web.XHR.FormData   as FD

import Data.Image         (Image, ImageArray)

class Monad m <= ManageMedia m where
  getImages   :: Pagination  -> m ImageArray
  uploadImage :: FD.FormData -> m (Maybe Image)

instance manageMediaHalogenM :: ManageMedia m => ManageMedia (HalogenM st act slots msg m) where
  getImages = lift <<< getImages
  uploadImage = lift <<< uploadImage
