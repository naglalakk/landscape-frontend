module CSS.Admin where

import Prelude

import CSS (Selector(..), element, hover, pseudo, select, (&), (?), (|>))
import CSS as CSS
import CSS.Common (center)
import CSS.Colors as Colors
import CSS.Font.Style as Font
import CSS.Geometry as CSS
import CSS.Media as Media
import CSS.Property as CSS
import CSS.Queries as Q
import CSS.Selector (with)
import CSS.Shared (button, container, flex, flexCenter, fullWidth, borderWhite, table, label)
import CSS.TextAlign as CSS
import CSS.Utils (padding)
import Data.NonEmpty (singleton)
import Halogen.HTML (HTML(..))
import Halogen.HTML.CSS as HCSS

stylesheet :: forall i p. HTML p i
stylesheet = HCSS.stylesheet do
  body
  container
  flex
  flexCenter
  fullWidth
  adminContainer
  adminMenu
  adminContent
  headings
  button
  link
  table
  label

body :: CSS.CSS
body =
  CSS.fromString "body" ? do
    CSS.fontFamily ["Lato"] (singleton CSS.sansSerif)
    CSS.backgroundColor Colors.black
    CSS.color Colors.white

headings :: CSS.CSS
headings =
  CSS.fromString "h1" ? do
    CSS.fontSize $ CSS.px 54.0
    CSS.marginBottom $ CSS.px 50.0

adminContainer :: CSS.CSS
adminContainer = 
  CSS.fromString ".admin-container" ? do
    CSS.paddingTop $ CSS.px 50.0

adminMenu :: CSS.CSS
adminMenu = 
  CSS.fromString ".admin-menu" ? do
    CSS.width $ CSS.px 200.0
    CSS.display CSS.flex
    CSS.flexDirection CSS.column

    CSS.fromString ".menu-link" ? do
      CSS.display CSS.flex
      CSS.color Colors.white
      CSS.textDecoration CSS.noneTextDecoration
      CSS.marginBottom $ CSS.px 25.0
      CSS.fontWeight $ CSS.FontWeight $ CSS.value Font.bold
      CSS.fontSize $ CSS.px 20.0
      CSS.key (CSS.fromString "cursor") "pointer"

      CSS.fromString "i" ? do
        CSS.paddingRight $ CSS.px 12.5

adminContent :: CSS.CSS
adminContent =
  CSS.fromString ".admin-content" ? do
    CSS.key (CSS.fromString "width") "calc(100% - 200px)"
    CSS.paddingLeft $ CSS.px 25.0
    CSS.paddingRight $ CSS.px 25.0

link :: CSS.CSS
link =
  CSS.fromString "a" ? do
    CSS.textDecoration CSS.noneTextDecoration
    CSS.color Colors.white
