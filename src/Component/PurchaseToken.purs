module Component.PurchaseToken where

import Prelude

import Component.HTML.Utils (css)
import Config (recvAddr)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Token (TokenId(..), TokenTransaction(..), TransactionStatus(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (ForkId(..))
import Resource.Token (class ManageToken, getTokenAmount, getTokenAmountByHash, getTokenTransaction, requestToken, updateTxStatus)
import Utils.Cardano (fromLovelace)

type State = 
  { tokenId :: Maybe TokenId
  , showAmount :: Boolean
  , errorMsg :: String
  , status :: Maybe TransactionStatus
  , tokenAmount :: Int
  , tokenTx :: Maybe TokenTransaction
  , minutes :: Int
  , seconds :: Int
  , clockFork :: Maybe ForkId
  , pollFork :: Maybe ForkId
  , heartbeat :: Boolean
  }

type Input = 
  { tokenId :: Maybe TokenId
  }

data Output =
  PurchaseWindowClose

data Action 
  = Initialize
  | Receive Input
  | Close
  | CancelRequest
  | Continue
  | ClockLoop
  | PollLoop

component :: forall q m
           . ManageToken m
          => MonadAff m
          => H.Component HH.HTML q Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction 
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
    }
  where
  initialState :: Input -> State
  initialState inp = 
    { tokenId: inp.tokenId
    , showAmount: false
    , errorMsg: ""
    , status: Nothing
    , tokenAmount: 0
    , tokenTx: Nothing
    , minutes: 0
    , seconds: 0
    , clockFork: Nothing
    , pollFork: Nothing
    , heartbeat: false
    }
  
  handleAction :: Action 
               -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Initialize -> pure unit
    CancelRequest -> do
      state <- H.get

      case state.tokenTx of
        Just (TokenTransaction tx) -> do
          tokenTx <- updateTxStatus tx.hash $ show Cancelled
          H.modify_ _ { tokenTx = tokenTx }
        Nothing -> pure unit
      H.modify_ _ { heartbeat = false 
                  , showAmount = false
                  }
      H.raise PurchaseWindowClose

    Close -> do
      H.raise PurchaseWindowClose

    -- Loop for countdown
    ClockLoop -> do
      state <- H.get 
      if state.minutes == 0 && state.seconds == 0 || not state.heartbeat
        then do
          H.modify_ _ { heartbeat = false }
          case state.clockFork of
            Just fId -> H.kill fId
            Nothing -> pure unit
        else do
          let
            newMinutes = 
              if state.minutes == 0 
                then 0 
                else 
                  if state.seconds == 0
                    then state.minutes - 1
                    else state.minutes
            newSeconds = 
              if state.seconds == 0
                then 59
                else state.seconds - 1
          H.modify_ _ { minutes = newMinutes
                      , seconds = newSeconds
                      }

          H.liftAff $ Aff.delay $ Aff.Milliseconds 1000.0
          handleAction ClockLoop

    PollLoop -> do
      state <- H.get
      case state.tokenTx of
        Just (TokenTransaction tokenTx) -> do
          refreshTx <- getTokenTransaction tokenTx.hash
          case refreshTx of
            Just (TokenTransaction rTx) -> 
              if rTx.status == Completed  || not state.heartbeat
                then do
                  H.modify_ _ { heartbeat = false 
                              , status = Just rTx.status
                              }
                  case state.pollFork of
                    Just fId -> H.kill fId
                    Nothing -> pure unit
                else do
                  H.modify_ _ { status = Just rTx.status }
            Nothing -> pure unit
        Nothing -> pure unit
      H.liftAff $ Aff.delay $ Aff.Milliseconds 5000.0
      handleAction PollLoop

    Continue -> do
      state <- H.get
      case state.tokenId of
        Just tId -> do
          reqToken <- requestToken tId
          case reqToken of
            Just (TokenTransaction tokenTx) -> do
              -- Fetch amount
              tokenAmount <- getTokenAmountByHash tokenTx.hash

              H.modify_ _ { minutes = 15, seconds = 0 
                          , tokenTx = Just $ TokenTransaction tokenTx
                          , heartbeat = true
                          }
              fId <- H.fork $ handleAction ClockLoop
              pId <- H.fork $ handleAction PollLoop
              H.modify_ _ { tokenAmount = fromMaybe 0 tokenAmount
                          , showAmount = true
                          , clockFork = Just fId
                          , pollFork = Just pId
                          , status = Just tokenTx.status
                          }
            Nothing -> H.modify_ _ { errorMsg = "Token not available" }
        Nothing -> pure unit
      pure unit
    Receive inp -> 
      H.modify_ _ 
        { tokenId = inp.tokenId
        }

  render :: State -> H.ComponentHTML Action () m
  render state = 
    HH.div
      [ css $ "flex flex-center purchase-container show-flex-" <> (show $ isJust state.tokenId )]
      [ HH.div
        [ css "container-padding container" ]
        [ case state.showAmount of
          false -> 
            HH.div
              []
              [ HH.p
                []
                [ HH.text "You are about to purchase 1 NFT. " ]
              , HH.p
                []
                [ HH.text "For more information on how to purchase NFTs on this site, refer to " 
                , HH.a
                  []
                  [ HH.text "this page" ]
                ]
              , HH.p
                []
                [ HH.text "Once you confirm the purchase you will get an address and exact amount of ADA that you will need to send to the address in order to receive your NFT.  Your request is reserved for 15 minutes. If the amount is not received in this timeframe the NFT is released and will be avaialble again for purchase." ]
              , HH.p
                []
                [ HH.text "Confirm the purchase by clicking “Continue” below." ]
              , HH.div
                [ css "error-message" ]
                [ HH.text state.errorMsg
                ]
              , HH.div
                [ css "flex flex-center actions" ]
                [ HH.button
                  [ css "button" 
                  , HE.onClick \_ -> Just Close ]
                  [ HH.text "Cancel" ]
                , HH.button
                  [ css "button" 
                  , HE.onClick \_ -> Just Continue ]
                  [ HH.text "Continue" ]
                ]
              ]
          true ->
            HH.div
              []
              [ HH.p
                [ css "bold text-center" ]
                [ HH.text "Your amount is: " ]
              , HH.p
                [ css "text-center amount" ]
                [ HH.text $ show $ fromLovelace state.tokenAmount 
                , HH.text " ₳"
                ]
              , HH.p
                [ css "text-center bold" ]
                [ HH.text "Send exactly this amount to the following address" ]
              , HH.div
                [ css "addr-container flex flex-center" ]
                [ HH.p
                  []
                  [ HH.text recvAddr ]
                ] 
              , HH.p
                [ css "text-center bold" ]
                [ HH.text "expires-in" ]
              , HH.div
                [ css "clock text-center" ]
                [ HH.h2
                  []
                  [ HH.text $ show state.minutes
                  , HH.text ":"
                  , if state.seconds < 10 
                      then HH.text $ "0" <> (show state.seconds)
                      else HH.text $ show state.seconds
                  ]
                ]
              , HH.p
                [ css "text-center bold" ]
                [ HH.text "Status:" ]
              , HH.p
                [ css "text-center" ]
                [ HH.text $ show state.status
                ]
              , HH.div
                [ css "flex flex-center" ]
                [ HH.button
                  [ css "button" 
                  , HE.onClick \_ -> Just CancelRequest
                  ]
                  [ case state.status of
                    Just Completed -> HH.text "Close"
                    _ -> HH.text "Cancel" 

                  ]
                ]
              ]
        ]
      ]
