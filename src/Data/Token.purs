module Data.Token where

import Prelude

import Data.Argonaut (decodeJson, encodeJson, jsonEmptyObject, (:=), (~>), (.:), (.:?))
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Image (Image)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (toLower)
import Formless as F
import Node.Stream (Read)
import Timestamp (Timestamp)

newtype TokenId = TokenId Int

derive instance newtypeTokenId :: Newtype TokenId _
derive instance genericTokenId :: Generic TokenId _
derive instance eqTokenId :: Eq TokenId
derive instance ordTokenId :: Ord TokenId

derive newtype instance encodeJsonTokenId :: EncodeJson TokenId
derive newtype instance decodeJsonTokenId :: DecodeJson TokenId

instance initialTokenId :: F.Initial TokenId where
  initial = TokenId 0

instance showTokenId :: Show TokenId where
  show = genericShow

newtype Token = Token
  { id :: TokenId
  , title :: String
  , policyId :: Maybe String
  , amount   :: Int
  , quantity :: Int
  , minted :: Int
  , available :: Int
  , metadata :: Maybe String
  , createdAt :: Timestamp
  , updatedAt :: Maybe Timestamp
  }

derive instance newtypeToken :: Newtype Token _
derive instance genericToken :: Generic Token _
derive instance eqToken :: Eq Token

derive newtype instance encodeJsonToken :: EncodeJson Token
instance decodeJsonToken :: DecodeJson Token where
  decodeJson json = do
    obj <- decodeJson json
    id  <- obj .: "id"
    title <- obj .: "title"
    policyId <- obj .:? "policyId"
    amount <- obj .: "amount"
    quantity <- obj .: "quantity"
    minted <- obj .: "minted"
    available <- obj .: "available"
    metadata <- obj .:? "metadata"
    createdAt <- obj .: "createdAt"
    updatedAt <- obj .:? "updatedAt"

    pure $ Token
      { id
      , title
      , policyId
      , amount
      , quantity
      , minted
      , available
      , metadata
      , createdAt
      , updatedAt
      }

newtype TokenTransactionId = TokenTransactionId Int

derive instance newtypeTokenTransactionId :: Newtype TokenTransactionId _
derive instance genericTokenTransactionId :: Generic TokenTransactionId _
derive instance eqTokenTransactionId :: Eq TokenTransactionId

derive newtype instance encodeJsonTokenTransactionId :: EncodeJson TokenTransactionId
derive newtype instance decodeJsonTokenTransactionId :: DecodeJson TokenTransactionId


-- | Status for TokenTransaction
data TransactionStatus
  = Request
  | Cancelled
  | Completed
  | Expired
  | Minting
  | Minted
  | Error String

derive instance genericTransactionStatus :: Generic TransactionStatus _

derive instance eqTransactionStatus :: Eq TransactionStatus

readTransactionStatus :: String -> TransactionStatus
readTransactionStatus "request" = Request
readTransactionStatus "cancelled" = Cancelled
readTransactionStatus "completed" = Completed
readTransactionStatus "expired" = Expired
readTransactionStatus "minting" = Minting
readTransactionStatus "minted" = Minted
readTransactionStatus x = Error x

instance showTransactionStatus :: Show TransactionStatus where
  show s = toLower $ genericShow s

instance encodeJsonTransactionStatus :: EncodeJson TransactionStatus where
  encodeJson status =
    case status of
      Error str -> encodeJson str
      _ -> encodeJson $ show status

instance decodeJsonTransactionStatus :: DecodeJson TransactionStatus where
  decodeJson json = do
    str <- decodeJson json
    pure $ readTransactionStatus str

newtype TokenTransaction = TokenTransaction
  { id :: TokenTransactionId
  , token :: Maybe Token
  , status :: TransactionStatus
  , hash :: String
  , txHash :: Maybe String
  , createdAt :: Timestamp
  , updatedAt :: Maybe Timestamp
  }

derive instance newtypeTokenTransaction :: Newtype TokenTransaction _
derive instance genericTokenTransaction :: Generic TokenTransaction _
derive instance eqTokenTransaction :: Eq TokenTransaction

instance encodeJsonTokenTransaction :: EncodeJson TokenTransaction where
  encodeJson (TokenTransaction tknTx)
    = "token" := map (\(Token token) -> token.id) tknTx.token
    ~> "status" := tknTx.status
    ~> "hash" := tknTx.hash
    ~> "txHash" := tknTx.txHash
    ~> "createdAt" := tknTx.createdAt
    ~> "updatedAt" := tknTx.updatedAt

derive newtype instance decodeJsonTokenTransaction :: DecodeJson TokenTransaction

instance showToken :: Show Token where
  show = genericShow
