module Resource.Token where

import Prelude

import Api.Endpoint (Status)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Token (Token, TokenId, TokenTransaction)
import Halogen (HalogenM, lift)

class Monad m <= ManageToken m where
  allTokens :: m (Array Token)
  getTokenById :: TokenId -> m (Maybe Token)
  getTokenAmount :: TokenId -> m (Maybe Int)
  getTokenAmountByHash :: String -> m (Maybe Int)
  createToken :: Token -> m (Maybe Token)
  updateToken :: Token -> m (Maybe Token)
  deleteToken :: TokenId -> m Unit
  requestToken :: TokenId -> m (Maybe TokenTransaction)
  updateTxStatus :: String -> String -> m (Maybe TokenTransaction)
  allTokenTransactions :: Status -> m (Array TokenTransaction)
  getTokenTransaction :: String -> m (Maybe TokenTransaction)
  updateTokenTransaction :: TokenTransaction -> m (Maybe TokenTransaction)


instance manageTokenHalogenM :: ManageToken m => ManageToken (HalogenM st act slots msg m) where
  allTokens = lift allTokens
  getTokenById = lift <<< getTokenById
  getTokenAmount = lift <<< getTokenAmount
  getTokenAmountByHash = lift <<< getTokenAmountByHash
  createToken = lift <<< createToken
  updateToken = lift <<< updateToken
  deleteToken = lift <<< deleteToken
  requestToken = lift <<< requestToken
  updateTxStatus hash = lift <<< updateTxStatus hash
  allTokenTransactions = lift <<< allTokenTransactions
  getTokenTransaction = lift <<< getTokenTransaction
  updateTokenTransaction = lift <<< updateTokenTransaction
