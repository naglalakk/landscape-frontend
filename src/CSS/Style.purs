module CSS.Style where

import Prelude

import CSS (Selector(..), element, hover, pseudo, select, (&), (?), (|>))
import CSS as CSS
import CSS.Colors as Colors
import CSS.Font.Style as Font
import CSS.Geometry as CSS
import CSS.Media as Media
import CSS.Property as CSS
import CSS.Queries as Q
import CSS.Selector (with)
import CSS.TextAlign as CSS
import CSS.Shared (button, container, containerWhite, containerPadding, relative, borderWhite, underlined, bold, textCenter, flex, flexCenter, flexWrap, column, spaceB, alignCenter, listStyleTypeNone, showElement, textRight, textLeft)
import CSS.Utils (padding)
import Data.NonEmpty (singleton)
import Halogen.HTML (HTML(..))
import Halogen.HTML.CSS as HCSS

stylesheet :: forall i p. HTML p i
stylesheet = HCSS.stylesheet do
  body
  a
  p
  h1
  h4
  header
  borderWhite
  -- Exhibition
  exhibition
  exhibitionItem
  instructionVisual
  purchaseContainer
  navigation
  overview
  overviewIcon

  -- Utility
  button
  container
  containerWhite
  containerPadding
  arrowDownWhite
  underlined
  relative
  bold
  textCenter
  textLeft
  textRight
  flex
  flexCenter
  flexWrap
  column
  spaceB
  alignCenter
  listStyleTypeNone
  showElement

body :: CSS.CSS
body =
  CSS.fromString "body" ? do
    CSS.fontFamily ["Lato"] (singleton CSS.sansSerif)
    CSS.backgroundColor Colors.black
    CSS.color Colors.white

a :: CSS.CSS
a = 
  CSS.fromString "a" ? do
    CSS.color Colors.white

p :: CSS.CSS
p = 
  CSS.fromString "p" ? do
    CSS.marginTop $ CSS.px 25.0
    CSS.marginBottom $ CSS.px 25.0
    CSS.lineHeight $ CSS.px 30.0
    CSS.fontSize $ CSS.px 18.0

h1 :: CSS.CSS
h1 = 
  CSS.fromString "h1" ? do
    CSS.fontSize $ CSS.px 54.0
    CSS.paddingBottom $ CSS.px 12.5
    CSS.key (CSS.fromString "display") "inline-block"

h4 :: CSS.CSS
h4 = CSS.fromString "h4" ? do
  CSS.fontSize $ CSS.px 28.0
  CSS.paddingBottom $ CSS.px 12.5
  CSS.key (CSS.fromString "display") "inline-block"

header :: CSS.CSS
header = 
  CSS.fromString ".header" ? do
    CSS.key (CSS.fromString "visibility") "hidden"
    CSS.position CSS.absolute
    CSS.top $ CSS.px 0.0
    CSS.left $ CSS.px 0.0
    CSS.width $ CSS.pct 100.0
    CSS.height $ CSS.px 100.0


-- White arrow down background
arrowDownWhite :: CSS.CSS
arrowDownWhite = 
  CSS.fromString ".arrow-down-white" ? do
    CSS.width $ CSS.px 121.0
    CSS.height $ CSS.px 63.0
    CSS.backgroundImage $ CSS.url "/static/img/arrow-down-white.png"
    CSS.backgroundRepeat CSS.noRepeat
    CSS.key (CSS.fromString "background-size") "100% 100%"

-- Exhibitions
exhibition :: CSS.CSS
exhibition = 
  CSS.fromString ".exhibition" ? do
    CSS.paddingTop $ CSS.px 100.0
    CSS.fromString ".title" ? do
      CSS.maxWidth $ CSS.px 720.0
      CSS.fontWeight $ CSS.FontWeight $ CSS.value Font.light
      CSS.textAlign CSS.leftTextAlign
      CSS.paddingBottom $ CSS.px 12.5

    CSS.fromString ".introduction" ? do
      CSS.maxWidth $ CSS.px 800.0
      CSS.key (CSS.fromString "margin") "0 auto"
      CSS.marginTop $ CSS.px 300.0
      CSS.marginBottom $ CSS.px 150.0
      CSS.fontSize $ CSS.px 20.0
      CSS.fontWeight $ CSS.FontWeight $ CSS.value Font.regular

    CSS.fromString ".navigation" ? do
      CSS.marginTop $ CSS.px 150.0

    CSS.fromString ".instruction-divider" ? do
      CSS.fontSize $ CSS.px 36.0
      CSS.paddingLeft $ CSS.px 25.0
      CSS.paddingRight $ CSS.px 25.0

exhibitionItem :: CSS.CSS
exhibitionItem =
  CSS.fromString ".exhibition-item" ? do
    CSS.paddingTop $ CSS.px 200.0
    CSS.fromString ".image" ? do
      CSS.key (CSS.fromString "object-fit") "contain"
      CSS.marginBottom $ CSS.px 25.0

    CSS.fromString ".info" ? do
      CSS.fromString ".title" ? do
        CSS.fontSize $ CSS.px 32.0

    CSS.fromString ".token-data" ? do
      CSS.fromString ".width-half" ? do
        CSS.key (CSS.fromString "width") "calc(50% - 50px)"

      CSS.fromString "ul" ? do
        CSS.fromString "li" ? do
          CSS.fontSize $ CSS.px 18.0
          CSS.marginBottom $ CSS.px 12.5

      CSS.fromString ".metadata" ? do

        CSS.fromString ".code" ? do
          CSS.fontFamily ["Source Code Pro"] (singleton CSS.sansSerif)
          CSS.border CSS.solid (CSS.px 1.0) Colors.white
          padding 12.5
          CSS.borderRadius 
            (CSS.px 10.0) 
            (CSS.px 10.0)
            (CSS.px 10.0)
            (CSS.px 10.0)
          CSS.key (CSS.fromString "white-space") "pre-wrap"


instructionVisual :: CSS.CSS
instructionVisual =
  CSS.fromString ".instruction-visual" ? do
    CSS.marginTop $ CSS.px 200.0
    CSS.marginBottom $ CSS.px 200.0

    CSS.fromString ".img-instruction" ? do
      CSS.height $ CSS.px 191.0
      CSS.key (CSS.fromString "width") "auto"

purchaseContainer :: CSS.CSS
purchaseContainer = 
  CSS.fromString ".purchase-container" ? do
    CSS.backgroundColor Colors.black
    CSS.position CSS.fixed
    CSS.width $ CSS.pct 100.0
    CSS.height $ CSS.pct 100.0
    CSS.top $ CSS.px 0.0
    CSS.left $ CSS.px 0.0

    CSS.fromString ".actions" ? do
      CSS.fromString "button" ? do
        CSS.marginRight $ CSS.px 25.0

    CSS.fromString ".amount" ? do
      CSS.fontSize $ CSS.px 40.0

    CSS.fromString ".addr-container" ? do
      CSS.border CSS.solid (CSS.px 1.0) Colors.white
      CSS.height $ CSS.px 100.0
      CSS.width $ CSS.pct 100.0

    CSS.fromString ".clock" ? do
      CSS.fontSize $ CSS.px 56.0

overview :: CSS.CSS
overview = 
  CSS.fromString ".overview" ? do
    CSS.position CSS.fixed
    CSS.zIndex 999
    CSS.width $ CSS.pct 100.0
    CSS.height $ CSS.pct 100.0
    CSS.top $ CSS.px 0.0
    CSS.left $ CSS.px 0.0
    CSS.backgroundImage $ CSS.url "/static/img/overviewbg.png" 
    CSS.backgroundRepeat CSS.noRepeat
    CSS.backgroundSize $ CSS.fromString "cover"
    CSS.backgroundPosition $ CSS.fromString "center"

    CSS.fromString ".artworks-container" ? do
      CSS.marginTop $ CSS.px 50.0
      CSS.fromString ".artwork" ? do
        CSS.marginBottom $ CSS.px 25.0

overviewIcon :: CSS.CSS
overviewIcon =
  CSS.fromString ".overview-icon" ? do
    CSS.height $ CSS.px 40.0

navigation :: CSS.CSS
navigation = 
  CSS.fromString ".navigation" ? do
    CSS.marginBottom $ CSS.px 100.0
