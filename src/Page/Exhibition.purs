module Page.Exhibition where

import Prelude

import CSS (offsetTop)
import Component.HTML.ExhibitionItem (exhibitionItem)
import Component.HTML.Overview (overview)
import Component.HTML.Utils (css, maybeElem)
import Component.PurchaseToken (Output(..))
import Component.PurchaseToken as PurchaseToken
import Data.Const (Const(..))
import Data.Exhibition (Exhibition(..), ExhibitionId)
import Data.Functor.Variant (SProxy(..))
import Data.Int (fromNumber)
import Data.Item (Item(..), ItemId(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Token (TokenId(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (logShow)
import Foreign.Window (scrollTo)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Resource.Exhibition (class ManageExhibition, getExhibitionById, getExhibitionItems)
import Resource.Token (class ManageToken)
import Utils.DOM (getTop)
import Web.HTML as HTML

type Input =
  { exhibitionId :: ExhibitionId
  }

type State = 
  { exhibition :: Maybe Exhibition
  , exhibitionId :: ExhibitionId
  , items :: Array Item
  , selectedTokenId :: Maybe TokenId
  , overviewOpen :: Boolean
  }

data Action 
  = Initialize
  | ToggleOverview
  | Purchase TokenId
  | HandlePurchaseAction PurchaseToken.Output
  | SelectItem ItemId

type ChildSlots =
  ( purchaseToken :: H.Slot (Const Void) PurchaseToken.Output Unit
  )

component :: forall q o m
           . MonadAff m
          => ManageExhibition m
          => ManageToken m
          => H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction 
      , initialize = Just Initialize
      }
    }
  where
  initialState :: Input -> State
  initialState inp = 
    { exhibition: Nothing
    , exhibitionId: inp.exhibitionId
    , items: []
    , selectedTokenId: Nothing
    , overviewOpen: false
    }
  
  handleAction :: Action 
               -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      exhibition <- getExhibitionById state.exhibitionId
      items <- getExhibitionItems state.exhibitionId

      H.modify_ _ { exhibition = exhibition 
                  , items = items
                  }
    HandlePurchaseAction output -> case output of
      PurchaseWindowClose -> H.modify_ _ { selectedTokenId = Nothing }
    Purchase tokenId -> 
      H.modify_ _ { selectedTokenId = Just tokenId }
    ToggleOverview -> do
      state <- H.get
      H.modify_ _ { overviewOpen = not state.overviewOpen }
    SelectItem itemId -> do
      let 
        identifier = "item-" <> (show $ unwrap itemId)
      H.modify_ _ { overviewOpen = false }

      top <- H.liftEffect $ getTop identifier
      case top of
        Just t -> do
          w <- H.liftEffect HTML.window
          H.liftEffect $ scrollTo 0 (fromMaybe 0 $ fromNumber t) w
        Nothing -> pure unit

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = 
    HH.div
      [ css "page-exhibition relative" ]
      [ overview 
        ToggleOverview 
        SelectItem 
        state.overviewOpen 
        state.items
      , HH.slot
        (SProxy :: SProxy "purchaseToken")
        unit
        PurchaseToken.component
        { tokenId: state.selectedTokenId }
        (Just <<< HandlePurchaseAction)
      , maybeElem state.exhibition \(Exhibition exhibition) ->
        HH.div
          [ css $ "exhibition container container-white show-" <> (show $ not state.overviewOpen) ]
          [ HH.div
            [ css "layer-1 container-padding" ]
            [ HH.h1
              [ css "underlined title" ]
              [ HH.text exhibition.title
              ]
            , HH.p
              [ css "text-center introduction" ]
              [ HH.text $ fromMaybe "" exhibition.introduction
              ]
            {--, HH.div
              [ css "navigation flex flex-center" ]
              [ HH.div
                [ css "arrow-down-white" ]
                []
              ]--}
            ]
          , HH.div
            [ css "layer-2 container-padding" ]
            [ {--HH.h1
              [ css "underlined" ]
              [ HH.text "Instructions" ]--}
              HH.div
              [ css "instruction-visual flex flex-center" ]
              [ HH.div
                [ css "flex column" ]
                [ HH.p
                  [ css "text-center bold" ]
                  [ HH.text "Navigate" ]
                , HH.img
                  [ css "img-instruction"
                  , HP.src "/static/img/mouse_scroll.png"
                  ]
                , HH.p
                  [ css "text-center" ]
                  [ HH.text "Scroll up / down" ]
                ]
              , HH.span
                [ css "instruction-divider" ]
                [ HH.text "or" ]
              , HH.div
                [ css "flex column" ]
                [ HH.p
                  [ css "text-center bold" ]
                  [ HH.text "Overview" ]
                , HH.img
                  [ css "img-instruction"
                  , HP.src "/static/img/overview.png"
                  ]
                , HH.p
                  [ css "text-center" ]
                  [ HH.text "Use the overview to view all artworks" ]
                ]
              ]
            , HH.div
              [ css "text-center sale"] 
              [ HH.h4
                [ css "bold underlined" ]
                [ HH.text "Sale" ]
              , HH.p
                []
                [ HH.text "During the exhibition, guests can purchase a unique copy of each artwork in the form of an NFT, minted on the Cardano blockchain." 
                , HH.br []
                , HH.text "In order to purchase a NFT you need to have access to a wallet that can send and receive ADA such as " 
                , HH.a
                  [ HP.href "https://daedaluswallet.io"]
                  [ HH.text "Dadaleus" ]
                , HH.text " or " 
                , HH.a
                  [ HP.href "https://yoroi-wallet.com" ]
                  [ HH.text "Yoroi." ]
                , HH.br []
                , HH.text "On purchase, guests will be assigned a 15 minute slot were the artwork is reserved until a payment is made." 
                , HH.br []
                , HH.text "If no payment is made in this period, the NFT is released and will be available again for purchase." 
                , HH.br []
                , HH.b
                  [ css "bold" ]
                  [ HH.text "Note that only a single copy of each artwork will be minted." ]
                , HH.br []
                ]
              ]
          , HH.div
            [ css "layer-3" ]
            (state.items <#> (exhibitionItem Purchase ToggleOverview))
          ]
        ]
      ]
