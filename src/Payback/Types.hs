{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Payback.Types where

import Data.Bifunctor
import Data.Hashable
import GHC.Generics (Generic)
import Data.Text
import Data.Aeson
import Data.ByteString.Lazy

data Transaction = Transaction
    { source :: User
    , metadata :: Metadata
    , reason :: Maybe Text
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
    deriving (Eq, Show, Generic, Ord)

instance FromJSON User
instance ToJSON User
instance Hashable User

data Amount = Amount
    { value :: Float
    , currency :: Currency
    } deriving (Eq, Show, Generic)

instance FromJSON Amount
instance ToJSON Amount

mkAmount :: Float -> Currency -> Amount
mkAmount = Amount

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

mkDateTime :: Date -> Time -> DateTime
mkDateTime = DateTime

data Date = Date
    { year :: Int
    , month :: Int
    , day :: Int
    } deriving (Show, Generic)

instance FromJSON Date
instance ToJSON Date

mkDate :: Int -> Int -> Int -> Date
mkDate = Date

data Time = Time
    { hour :: Int
    , minutes :: Int
    } deriving (Show, Generic)

instance FromJSON Time
instance ToJSON Time

mkTime :: Int -> Int -> Time
mkTime = Time