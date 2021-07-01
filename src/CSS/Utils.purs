module CSS.Utils where

import Prelude
import CSS  as CSS
import Data.Maybe (Maybe)
import Data.NonEmpty (singleton)


padding :: Number -> CSS.CSS
padding size 
  = CSS.padding 
    (CSS.px size)
    (CSS.px size)
    (CSS.px size)
    (CSS.px size)

margin :: Number -> CSS.CSS
margin size 
  = CSS.margin
    (CSS.px size)
    (CSS.px size)
    (CSS.px size)
    (CSS.px size)

fontFace :: String 
         -> String
         -> Number
         -> CSS.FontStyle
         -> Maybe CSS.FontFaceFormat
         -> CSS.CSS
fontFace family url weight style format = CSS.fontFace do
  CSS.fontFamily [family] (singleton CSS.sansSerif)
  CSS.fontFaceSrc (singleton $ CSS.FontFaceSrcUrl url format)
  CSS.fontWeight $ CSS.weight weight
  CSS.fontStyle style


-- | Background cover div 
--   The src of the background image is passed
backgroundCover :: String -> CSS.CSS
backgroundCover src = do
  CSS.backgroundRepeat CSS.noRepeat
  CSS.backgroundImage $ CSS.url src
  CSS.backgroundSize CSS.cover
  CSS.backgroundPosition $ CSS.placed CSS.sideCenter CSS.sideCenter
