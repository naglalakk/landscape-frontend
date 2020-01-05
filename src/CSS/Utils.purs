module CSS.Utils where

import Prelude
import Color                    as Color
import CSS                      as CSS
import CSS                      ((&),(?))
import CSS.Common               (center)
import Halogen.HTML.CSS         as HCSS


-- | Background cover div 
--   The src of the background image is passed
backgroundCover :: String -> CSS.CSS
backgroundCover src = do
  CSS.backgroundRepeat CSS.noRepeat
  CSS.backgroundImage $ CSS.url src
  CSS.backgroundSize CSS.cover
  CSS.backgroundPosition $ CSS.placed CSS.sideCenter CSS.sideCenter
