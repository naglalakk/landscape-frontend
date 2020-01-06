module Data.User where

import Prelude
import Data.Argonaut.Encode     (class EncodeJson)
import Data.Argonaut.Decode     (class DecodeJson)
import Data.Generic.Rep         (class Generic)
import Data.Generic.Rep.Show    (genericShow)
import Data.Newtype             (class Newtype)
import Formless                 as F

import Data.Email               (Email)

newtype Auth = Auth 
  { username :: Username
  , password :: String
  }

derive instance newtypeAuth :: Newtype Auth _
derive instance genericAuth :: Generic Auth _
derive instance eqAuth :: Eq Auth
derive instance ordAuth :: Ord Auth

derive newtype instance encodeJsonAuth :: EncodeJson Auth
derive newtype instance decodeJsonAuth :: DecodeJson Auth

instance showAuth :: Show Auth where
  show = genericShow

newtype UserId = UserId Int

derive instance newtypeUserId :: Newtype UserId _
derive instance genericUserId :: Generic UserId _
derive instance eqUserId :: Eq UserId
derive instance ordUserId :: Ord UserId

derive newtype instance encodeJsonUserId :: EncodeJson UserId
derive newtype instance decodeJsonUserId :: DecodeJson UserId

instance showUserId :: Show UserId where
  show = genericShow

instance initialUserId :: F.Initial UserId where
  initial = UserId 0

newtype Username = Username String

derive instance newtypeUsername :: Newtype Username _
derive instance genericUsername :: Generic Username _
derive instance eqUsername :: Eq Username
derive instance ordUsername :: Ord Username

derive newtype instance encodeJsonUsername :: EncodeJson Username
derive newtype instance decodeJsonUsername :: DecodeJson Username

instance showUsername :: Show Username where
  show = genericShow

-- | User type. 
--   We do not keep the password
--   within this data structure for safety
--   reasons.
newtype User = User 
  { id       :: UserId
  , username :: Username
  , email    :: Email
  , admin    :: Boolean
  }

derive instance newtypeUser :: Newtype User _
derive instance genericUser :: Generic User _
derive instance eqUser :: Eq User
derive instance ordUser :: Ord User

derive newtype instance encodeJsonUser :: EncodeJson User
derive newtype instance decodeJsonUser :: DecodeJson User

instance showUser :: Show User where
  show = genericShow
