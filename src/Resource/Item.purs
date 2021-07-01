module Resource.Item where

import Prelude

import Data.Item (ItemId, Item)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= ManageItem m where
  allItems :: m (Array Item)
  getItemById :: ItemId -> m (Maybe Item)
  createItem :: Item -> m (Maybe Item)
  updateItem :: Item -> m (Maybe Item)
  deleteItem :: ItemId -> m Unit


instance manageItemHalogenM :: ManageItem m => ManageItem (HalogenM st act slots msg m) where
  allItems = lift allItems
  getItemById = lift <<< getItemById
  createItem = lift <<< createItem
  updateItem = lift <<< updateItem
  deleteItem = lift <<< deleteItem
