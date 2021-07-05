module Component.HTML.ExhibitionItem where

import Prelude

import Component.HTML.Utils (css, maybeElem)
import Data.Exhibition (Exhibition(..))
import Data.Image (Image(..))
import Data.Item (Item(..), ItemId(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Token (Token(..), TokenId(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Utils.Text (shorten)

exhibitionItem :: forall props act
                . (TokenId -> act)
               -> act
               -> Item
               -> HH.HTML props act
exhibitionItem purchaseAction triggerOverview (Item item) = 
  HH.div 
    [ css "exhibition-item" 
    , HP.id_ $ "item-" <> (show $ unwrap item.id)
    ] 
    [ maybeElem item.image \(Image img) ->
      HH.div
      []
      [ HH.div
        [ css "flex flex-center image" ]
        [ HH.img
          [ HP.src img.src
          ]
        ]
      {--, HH.p
        [ css "text-center" ]
        [ HH.text "( Click image for fullscreen )" ]--}
      ]
    , HH.div
      [ css "info underlined" ]
      [ HH.div
        [ css "flex space-between align-center" ]
        [ HH.p
          [ css "title" ]
          [ HH.text item.title ]
        , HH.div
          [ HE.onClick \_ -> Just triggerOverview ]
          [ HH.img
            [ HP.src "/static/img/overview.png" 
            , css "overview-icon" 
            ]
          ]
        ]
      ]
    ,  maybeElem item.token \(Token token) ->
      HH.div
        [ css "token-data" ]
        [ HH.div
          [ css "flex space-between data-info" ]
          [ HH.div
            [ css "info width-half" ]
            [ HH.p
              []
              [ HH.text "Token info" ]
            , HH.div
              [ css "flex space-between" ]
              [ HH.ul
                [ css "list-style-type-none text-left" ]
                [ HH.li
                  []
                  [ HH.text "Title" ]
                , HH.li
                  []
                  [ HH.text "PolicyId" ]
                , HH.li
                  []
                  [ HH.text "Quantity" ]
                , HH.li
                  []
                  [ HH.text "Minted" ]
                , HH.li
                  []
                  [ HH.text "Available" ]
                ]
              , HH.ul
                [ css "list-style-type-none text-right" ]
                [ HH.li
                  []
                  [ HH.text token.title ]
                , HH.li
                  []
                  [ HH.a
                    [ HP.href "https://github.com/naglalakk/cardano-worker/blob/main/policy/policy.json" ]
                    [ HH.text $ shorten 8 $ fromMaybe "NA" token.policyId ]]
                , HH.li
                  []
                  [ HH.text $ show token.quantity ]
                , HH.li
                  []
                  [ HH.text $ show token.minted ]
                , HH.li
                  []
                  [ HH.text $ show token.available ]
                ]
              ]
            , HH.p
              [ css "text-left" ]
              [ HH.text "Price: 1000 ADA" ]
            ]
          , HH.div
            [ css "metadata width-half" ]
            [ HH.p
              []
              [ HH.text "Metadata" ]
            , HH.p
              [ css "code text-left" ]
              [ case token.metadata of
                Just md -> HH.text md
                Nothing -> HH.text "No Metadata"
              ]
            ]
          ]
        , HH.div
          [ css "flex" ]
          [ if (token.quantity - token.minted) > 0
            then
              HH.button
                [ css "button" 
                , HE.onClick \_ -> Just $ purchaseAction token.id
                ]
                [ HH.text "Purchase" ]
            else
              HH.button
                [ css "button disabled" ]
                [ HH.text "Sold out" ]
          ]
        ]
    ]
