{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Bifunctor
import Data.Hashable
import GHC.Generics (Generic)
import Data.Text
import Data.Aeson
import Data.ByteString.Lazy (ByteString)

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
    deriving (Eq, Generic, Ord)

instance Show User where
    show = unpack . name

instance FromJSON User
instance ToJSON User
instance Hashable User

data Amount = Amount
    { value :: Float
    , currency :: Currency
    } deriving (Eq, Generic)

instance Show Amount where
    show (Amount v c) = show v ++ show c

instance FromJSON Amount
instance ToJSON Amount

data Currency = EUR | CHF
    deriving (Eq,Generic)

instance Show Currency where
    show EUR = "€"
    show CHF = "CHF"

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
