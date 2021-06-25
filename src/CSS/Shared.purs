module CSS.Shared where

import Prelude
import CSS.Colors as Colors
import CSS.ListStyle.Type as CSS
import CSS.Selector (with)
import CSS as CSS
import CSS ((?), hover)
import CSS.Border as CSS
import CSS.Common (center)
import CSS.Media as Media
import CSS.Queries as Q
import CSS.TextAlign as CSS
import CSS.Utils (padding)
import Data.NonEmpty (singleton)

container :: CSS.CSS
container = do
  CSS.fromString ".container" ? do
    CSS.key (CSS.fromString "margin") "0 auto"
    CSS.width $ CSS.px Q.desktopWide

    CSS.query Media.screen (singleton $ Media.maxWidth $ CSS.px Q.desktopWide) $ do
      CSS.width $ CSS.px Q.desktop

    CSS.query Media.screen (singleton $ Media.maxWidth $ CSS.px Q.desktop) $ do
      CSS.width $ CSS.px Q.tablet
    CSS.query Media.screen (singleton $ Media.maxWidth $ CSS.px Q.tablet) $ do
      CSS.width $ CSS.px Q.mobile

-- TODO: wrong naming
containerPadding :: CSS.CSS
containerPadding = 
  CSS.fromString ".container-padding" ? do
    CSS.marginLeft $ CSS.px 25.0
    CSS.marginRight $ CSS.px 25.0

containerWhite :: CSS.CSS
containerWhite = 
  CSS.fromString ".container-white" ? do
    CSS.borderLeft CSS.solid (CSS.px 1.0) Colors.white
    CSS.borderRight CSS.solid (CSS.px 1.0) Colors.white

fullWidth :: CSS.CSS
fullWidth =
  CSS.fromString ".full-width" ? do
    CSS.width $ CSS.pct 100.0

borderWhite :: CSS.CSS
borderWhite = 
  CSS.fromString ".border-white" ? do
    CSS.borderLeft CSS.solid (CSS.px 1.0) Colors.white
    CSS.borderRight CSS.solid (CSS.px 1.0) Colors.white

flex :: CSS.CSS
flex =
  CSS.fromString ".flex" ? do
    CSS.display CSS.flex

flexCenter :: CSS.CSS
flexCenter = 
  CSS.fromString ".flex-center" ? do
    CSS.display CSS.flex
    CSS.justifyContent $ CSS.JustifyContentValue center
    CSS.alignItems center

flexWrap :: CSS.CSS
flexWrap =
  CSS.fromString ".flex-wrap" ? do
    CSS.flexWrap CSS.wrap

column :: CSS.CSS
column =
  CSS.fromString ".column" ? do
    CSS.flexDirection CSS.column

spaceB :: CSS.CSS
spaceB = 
  CSS.fromString ".space-between" ? do
    CSS.justifyContent CSS.spaceBetween


button :: CSS.CSS
button = do
  CSS.fromString ".button" ? do
    CSS.color Colors.white
    CSS.marginTop $ CSS.px 25.0
    CSS.marginBottom $ CSS.px 25.0
    CSS.width $ CSS.px 180.0
    CSS.height $ CSS.px 40.0
    CSS.border CSS.solid (CSS.px 1.0) Colors.white
    CSS.backgroundColor Colors.black
    CSS.textAlign CSS.center

  CSS.fromString "a.button" ? do
    CSS.display CSS.block
    CSS.lineHeight $ CSS.px 40.0
    CSS.textDecoration CSS.noneTextDecoration


  CSS.fromString ".button" `with` hover ? do
    CSS.backgroundColor Colors.white
    CSS.color Colors.black


table :: CSS.CSS
table =
  CSS.fromString "table" ? do
    CSS.fromString "thead" ? do
      CSS.fromString "tr" ? do
        CSS.borderBottom CSS.solid (CSS.px 1.0) Colors.white

    CSS.fromString "tbody" ? do
      CSS.fromString "td" ? do
        CSS.textAlign $ CSS.TextAlign center
        
        CSS.borderBottom CSS.solid (CSS.px 1.0) Colors.white


    CSS.fromString "td,th" ? do
      padding 10.0

label :: CSS.CSS
label =
  CSS.fromString "label" ? do
    CSS.display CSS.block
    CSS.marginTop $ CSS.px 12.5
    CSS.marginBottom $ CSS.px 12.5
    CSS.marginBottom $ CSS.px 12.5

underlined :: CSS.CSS
underlined = 
  CSS.fromString ".underlined" ? do
    CSS.borderBottom CSS.solid (CSS.px 1.0) Colors.white

bold :: CSS.CSS
bold = 
  CSS.fromString ".bold" ? do
    CSS.fontWeight CSS.bold

textCenter :: CSS.CSS
textCenter =
  CSS.fromString ".text-center" ? do
    CSS.textAlign $ CSS.TextAlign center


relative :: CSS.CSS
relative = 
  CSS.fromString ".relative" ? do
    CSS.position CSS.relative


textRight :: CSS.CSS
textRight = CSS.fromString ".text-right" ? do
  CSS.textAlign CSS.rightTextAlign

textLeft :: CSS.CSS
textLeft = 
  CSS.fromString ".text-left" ? do
    CSS.textAlign CSS.leftTextAlign

listStyleTypeNone :: CSS.CSS
listStyleTypeNone =
  CSS.fromString ".list-style-type-none" ? do
    CSS.listStyleType CSS.None


alignCenter :: CSS.CSS
alignCenter = 
  CSS.fromString ".align-center" ? do
    CSS.alignItems center

showElement :: CSS.CSS
showElement = do
  CSS.fromString ".show-true" ? do
    CSS.display CSS.block

  CSS.fromString ".show-false" ? do
    CSS.display CSS.displayNone

  CSS.fromString ".show-flex-true" ? do
    CSS.display CSS.flex
  
  CSS.fromString ".show-flex-false" ? do
    CSS.display CSS.displayNone
