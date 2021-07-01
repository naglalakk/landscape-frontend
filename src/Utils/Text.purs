module Utils.Text where

import Prelude
import Data.String (take)


shorten :: Int -> String -> String
shorten n str = bit <> "..."
  where
    bit = take n str
