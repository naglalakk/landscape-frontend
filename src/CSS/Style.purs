module CSS.Style where

import Prelude

import CSS (Selector(..), element, hover, pseudo, select, (&), (?), (|>))
import CSS as CSS
import CSS.Colors as Colors
import CSS.Common (center)
import CSS.Font.Style as Font
import CSS.Geometry as CSS
import CSS.Media as Media
import CSS.Property as CSS
import CSS.Queries as Q
import CSS.Selector (with)
import CSS.TextAlign as CSS
import CSS.Shared (button, container, containerWhite, containerPadding, relative, borderWhite, boxWhite, underlined, bold, textCenter, flex, flexCenter, flexWrap, flexEnd, column, spaceB, alignCenter, listStyleTypeNone, showElement, textRight, textLeft, breathe)
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
  boxWhite
  -- Pages
  pageHome

  -- Exhibitions
  pageExhibitions
  exhibitions

  -- Exhibition
  exhibition
  exhibitionItem
  instructionVisual
  purchaseContainer
  navigation
  overview
  overviewIcon
  sale

  -- About 
  pageAbout

  -- Elements
  coverImage
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
  flexEnd
  column
  spaceB
  alignCenter
  listStyleTypeNone
  showElement
  breathe
  -- overflowYShow

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
    -- CSS.letterSpacing $ CSS.px 1.2
    CSS.fontFamily ["Open Sans"] (singleton CSS.sansSerif)

h1 :: CSS.CSS
h1 = 
  CSS.fromString "h1" ? do
    CSS.fontSize $ CSS.px 54.0
    CSS.paddingBottom $ CSS.px 12.5
    CSS.key (CSS.fromString "display") "inline-block"
    CSS.query Media.screen (singleton $ Media.maxWidth $ CSS.px  767.0) $ do
      CSS.fontSize $ CSS.px 28.0

h4 :: CSS.CSS
h4 = CSS.fromString "h4" ? do
  CSS.fontSize $ CSS.px 28.0
  CSS.paddingBottom $ CSS.px 12.5
  CSS.key (CSS.fromString "display") "inline-block"

header :: CSS.CSS
header = 
  CSS.fromString ".header" ? do
    CSS.position CSS.absolute
    CSS.top $ CSS.px 0.0
    CSS.left $ CSS.px 0.0
    CSS.width $ CSS.pct 100.0
    CSS.height $ CSS.px 100.0
    CSS.zIndex 999

    CSS.fromString ".menu" ? do
      CSS.fontFamily ["Oswald"] (singleton CSS.sansSerif)
      CSS.fontSize $ CSS.px 18.0
      CSS.paddingRight $ CSS.px 25.0

      CSS.fromString ".menu-item" ? do
        CSS.textDecoration CSS.noneTextDecoration
        CSS.key (CSS.fromString "cursor") "pointer"


      CSS.fromString ".divider" ? do
        CSS.marginLeft $ CSS.px 12.5
        CSS.marginRight $ CSS.px 12.5


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
      CSS.fontFamily ["Lato"] (singleton CSS.sansSerif)
      CSS.maxWidth $ CSS.px 800.0
      CSS.key (CSS.fromString "margin") "0 auto"
      CSS.marginTop $ CSS.px 300.0
      CSS.marginBottom $ CSS.px 125.0
      CSS.fontSize $ CSS.px 20.0
      CSS.fontWeight $ CSS.FontWeight $ CSS.value Font.regular

    CSS.fromString ".instruction-divider" ? do
      CSS.fontSize $ CSS.px 36.0
      CSS.paddingLeft $ CSS.px 25.0
      CSS.paddingRight $ CSS.px 25.0

exhibitionItem :: CSS.CSS
exhibitionItem =
  CSS.fromString ".exhibition-item" ? do
    CSS.paddingTop $ CSS.px 50.0
    CSS.fromString ".image" ? do
      CSS.key (CSS.fromString "object-fit") "contain"
      CSS.marginBottom $ CSS.px 25.0

      CSS.fromString "img" ? do
        CSS.maxWidth $ CSS.pct 100.0

    CSS.fromString ".info" ? do
      CSS.fromString ".title" ? do
        CSS.fontSize $ CSS.px 32.0
        CSS.fontFamily ["Lato"] (singleton CSS.sansSerif)

    CSS.fromString ".token-data" ? do
      CSS.fromString ".data-info" ? do
        CSS.query Media.screen (singleton $ Media.maxWidth $ CSS.px  767.0) $ do
          CSS.flexDirection CSS.column

      CSS.fromString ".width-half" ? do
        CSS.key (CSS.fromString "width") "calc(50% - 50px)"
        
        CSS.query Media.screen (singleton $ Media.maxWidth $ CSS.px  767.0) $ do
          CSS.width $ CSS.pct 100.0

      CSS.fromString "ul" ? do
        CSS.fromString "li" ? do
          CSS.fontSize $ CSS.px 18.0
          CSS.marginBottom $ CSS.px 12.5

      CSS.fromString ".metadata" ? do

        CSS.fromString ".code" ? do
          CSS.fontSize $ CSS.px 12.0
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
    CSS.key (CSS.fromString "display") "none !important"

    CSS.marginTop $ CSS.px 200.0
    CSS.marginBottom $ CSS.px 200.0

    CSS.fromString ".img-instruction" ? do
      CSS.height $ CSS.px 191.0
      CSS.key (CSS.fromString "width") "auto"

    CSS.query Media.screen (singleton $ Media.maxWidth $ CSS.px  767.0) $ do
      CSS.flexDirection CSS.column

purchaseContainer :: CSS.CSS
purchaseContainer = 
  CSS.fromString ".purchase-container" ? do
    CSS.key (CSS.fromString "overflow-y") "scroll"
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

      
      CSS.query Media.screen (singleton $ Media.maxWidth $ CSS.px  767.0) $ do
        CSS.fromString ".address" ? do
          CSS.fontSize $ CSS.px 8.0

    CSS.fromString ".clock" ? do
      CSS.fontSize $ CSS.px 56.0

overview :: CSS.CSS
overview = 
  CSS.fromString ".overview" ? do
    CSS.position CSS.fixed
    -- CSS.key (CSS.fromString "overflow-y") "scroll"
    CSS.zIndex 1000
    CSS.width $ CSS.pct 100.0
    CSS.height $ CSS.pct 100.0
    CSS.top $ CSS.px 0.0
    CSS.left $ CSS.px 0.0
    CSS.backgroundColor Colors.black
    CSS.backgroundImage $ CSS.url "/static/img/overviewbg.png" 
    CSS.backgroundRepeat CSS.noRepeat
    CSS.backgroundSize $ CSS.fromString "cover"
    CSS.backgroundPosition $ CSS.fromString "center"

    CSS.fromString ".artworks-container" ? do
      CSS.marginTop $ CSS.px 50.0
      CSS.fromString ".artwork" ? do
        CSS.marginBottom $ CSS.px 25.0
        
        CSS.fromString "img" ? do
          CSS.maxWidth $ CSS.pct 100.0

      
      CSS.query Media.screen (singleton $ Media.maxWidth $ CSS.px  767.0) $ do
        CSS.flexDirection CSS.column

overviewIcon :: CSS.CSS
overviewIcon =
  CSS.fromString ".overview-icon" ? do
    CSS.display CSS.displayNone
    CSS.height $ CSS.px 40.0

navigation :: CSS.CSS
navigation = 
  CSS.fromString ".navigation" ? do
    CSS.marginBottom $ CSS.px 100.0


pageHome :: CSS.CSS
pageHome =
  CSS.fromString ".page-home" ? do
    CSS.paddingTop $ CSS.px 100.0


coverImage :: CSS.CSS
coverImage = 
  CSS.fromString ".cover-image" ? do
    CSS.width $ CSS.pct 100.0
    CSS.height $ CSS.px 800.0
    CSS.backgroundRepeat CSS.noRepeat
    CSS.backgroundAttachment $ CSS.fromString "fixed"
    CSS.backgroundSize $ CSS.fromString "cover" 
    CSS.backgroundPosition $ CSS.fromString "center center"

    CSS.fromString ".title-bar" ? do
      CSS.position CSS.absolute
      CSS.bottom $ CSS.px 0.0
      CSS.left $ CSS.px 0.0
      CSS.width $ CSS.pct 100.0
      CSS.height $ CSS.px 100.0
      CSS.key (CSS.fromString "background") "linear-gradient(to bottom, transparent 0%, rgb(3, 12, 34) 100%);"

    CSS.fromString ".title" ? do
      CSS.paddingBottom $ CSS.px 25.0

-- Exhibitions
pageExhibitions :: CSS.CSS
pageExhibitions = 
  CSS.fromString ".page-exhibitions" ? do
    CSS.paddingTop $ CSS.px 100.0

exhibitions :: CSS.CSS
exhibitions =
  CSS.fromString ".exhibitions" ? do
    CSS.marginTop $ CSS.px 25.0

    CSS.fromString ".featured" `with` CSS.fromString ".exhibition" ? do
      CSS.flexBasis $ CSS.pct 100.0
      CSS.height $ CSS.px 450.0

    CSS.fromString ".exhibition" `with` hover ? do
      CSS.fromString ".info" ? do
        CSS.opacity 1.0

    CSS.fromString ".exhibition" ? do
      CSS.height $ CSS.px 332.0
      CSS.key (CSS.fromString "cursor") "pointer"
      CSS.key (CSS.fromString "flex-basis") "calc(50% - 25px)"
      CSS.backgroundRepeat CSS.noRepeat
      CSS.backgroundAttachment $ CSS.fromString "fixed"
      CSS.backgroundSize $ CSS.fromString "cover" 
      CSS.backgroundPosition $ CSS.fromString "center center"
      CSS.marginBottom $ CSS.px 25.0

      CSS.fromString ".info" ? do
        CSS.opacity 0.0
        CSS.position CSS.absolute
        CSS.left $ CSS.px 0.0
        CSS.bottom $ CSS.px 0.0
        CSS.width $ CSS.pct 100.0
        CSS.height $ CSS.px 160.0
        CSS.key (CSS.fromString "background") "linear-gradient(to bottom, transparent 0%, rgb(3, 12, 34) 100%);"
        CSS.key (CSS.fromString "transition") "all 0.6s ease-in-out"

        CSS.fromString ".title" ? do
          CSS.fontSize $ CSS.px 36.0
          CSS.paddingLeft $ CSS.px 25.0
          CSS.paddingRight $ CSS.px 25.0

        CSS.fromString ".start-date" ? do
          CSS.paddingLeft $ CSS.px 25.0

pageAbout :: CSS.CSS
pageAbout =
  CSS.fromString ".page-about" ? do
    CSS.paddingTop $ CSS.px 100.0
    CSS.key (CSS.fromString "min-height") "100vh"

    {--CSS.fromString ".about-section" ? do
      CSS.fromString "p" ? do
        CSS.fontFamily ["Lato"] (singleton CSS.sansSerif)--}


overflowYShow :: CSS.CSS
overflowYShow = do
  CSS.fromString ".overflow-y-true" ? do
    CSS.key (CSS.fromString "overflow-y") "scroll"
  CSS.fromString ".overflow-y-false" ? do
    CSS.key (CSS.fromString "overflow-y") "hidden"

sale ::  CSS.CSS
sale =
  CSS.fromString ".sale" ? do
    CSS.marginTop $ CSS.px 200.0
