module Utils.Site where

import Prelude
import Config               (environment)
import Data.Environment     (toEnvironment, getSiteURL)

siteURL :: String
siteURL = getSiteURL $ toEnvironment environment
