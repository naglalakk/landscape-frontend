module Resource.Exhibition where

import Prelude

import Data.Exhibition (ExhibitionId, Exhibition)
import Data.Item (Item)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= ManageExhibition m where
  allExhibitions :: m (Array Exhibition)
  getExhibitionById :: ExhibitionId -> m (Maybe Exhibition)
  getExhibitionItems :: ExhibitionId -> m (Array Item)
  createExhibition :: Exhibition -> m (Maybe Exhibition)
  updateExhibition :: Exhibition -> m (Maybe Exhibition)
  deleteExhibition :: ExhibitionId -> m Unit


instance manageExhibitionHalogenM :: ManageExhibition m => ManageExhibition (HalogenM st act slots msg m) where
  allExhibitions = lift allExhibitions
  getExhibitionById = lift <<< getExhibitionById
  getExhibitionItems = lift <<< getExhibitionItems
  createExhibition = lift <<< createExhibition
  updateExhibition = lift <<< updateExhibition
  deleteExhibition = lift <<< deleteExhibition
