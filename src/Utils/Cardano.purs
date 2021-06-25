module Utils.Cardano where

import Prelude
import Data.Int (toNumber)

fromLovelace :: Int -> Number
fromLovelace lovelace = (toNumber lovelace) / 1000000.0
