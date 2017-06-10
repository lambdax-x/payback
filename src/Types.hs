{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import Data.Text
import Data.Aeson
import Data.ByteString.Lazy

mkTx :: ByteString -> Maybe Transaction
mkTx = decode

data Transaction = Transaction
    { source :: User
    , metadata :: Metadata
    , reason :: Text
    , debtors :: [(User, Amount)]
    , grantees :: [(User, Amount)]
    } deriving (Show, Generic)

instance FromJSON Transaction
instance ToJSON Transaction

data Metadata = Metadata
    { datetime :: DateTime
    , location :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON Metadata
instance ToJSON Metadata

newtype User = User { name :: Text }
    deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User

data Amount = Amount
    { value :: Float
    , currency :: Currency
    } deriving (Eq, Show, Generic)

instance FromJSON Amount
instance ToJSON Amount

data Currency = EUR | CHF
    deriving (Eq, Show, Generic)

instance FromJSON Currency where
    parseJSON (String "EUR") = return EUR
    parseJSON (String "€") = return EUR
    parseJSON (String "CHF") = return CHF
    parseJSON _ = error "invalid currency"

instance ToJSON Currency where
    toJSON EUR = String "€"
    toJSON CHF = String "CHF"

data DateTime = DateTime
    { date :: Date
    , time :: Time
    } deriving (Show, Generic)

instance FromJSON DateTime
instance ToJSON DateTime

data Date = Date
    { day :: Int
    , month :: Int
    , year :: Int
    } deriving (Show, Generic)

instance FromJSON Date
instance ToJSON Date

data Time = Time
    { hour :: Int
    , minutes :: Int
    } deriving (Show, Generic)

instance FromJSON Time
instance ToJSON Time
