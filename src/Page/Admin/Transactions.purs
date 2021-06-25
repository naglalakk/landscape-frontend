module Page.Admin.Transactions where

import Prelude

import Component.HTML.Utils (css)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Token (Token(..), TokenTransaction(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Resource.Token (class ManageToken, allTokenTransactions)
import Timestamp (Timestamp(..), formatToDateTimeShortStr)

type State = 
  { txs :: Array TokenTransaction
  }

data Action 
  = Initialize

component :: forall q i o m
           . ManageToken m
          => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction 
      , initialize = Just Initialize
      }
    }
  where
  initialState :: State
  initialState = 
    { txs: []
    }
  
  handleAction :: Action 
               -> H.HalogenM State Action () o m Unit
  handleAction = case _ of
    Initialize -> do
      txs <- allTokenTransactions { status: Nothing }
      H.modify_ _ { txs = txs }

  render :: State -> H.ComponentHTML Action () m
  render state = 
    HH.div
      []
      [ HH.h1
        []
        [ HH.text "Transactions" ]
      , HH.table
        [ css "table responsive-table" ]
        [ HH.thead
          []
          [ HH.tr
            []
            [ HH.th 
              []
              [ HH.text "Token" ]
            , HH.th
              []
              [ HH.text "Status" ]
            , HH.th
              []
              [ HH.text "Created" ]
            , HH.th
              []
              [ HH.text "Last updated" ]
            ]
          ]
        , HH.tbody
          []
          (map (\(TokenTransaction tokenTx) -> 
            HH.tr
              []
              [ HH.td
                []
                [ HH.text $ fromMaybe "" $ map (\(Token tkn) -> tkn.title) tokenTx.token
                ]
              , HH.td
                []
                [ HH.text $ show tokenTx.status ]
              , HH.td
                []
                [ HH.text $ formatToDateTimeShortStr tokenTx.createdAt
                ]
              , HH.td
                []
                [ HH.text $ fromMaybe "" $ map (\stamp -> formatToDateTimeShortStr stamp) tokenTx.updatedAt 
                ]
              ] 
            ) state.txs)
        ]
      ]
