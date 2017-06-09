module Types where

import Data.Text

data Transaction = Transaction
    { source :: User
    , metaData :: MetaData
    , reason :: Text
    , debtors :: [(User, Amount)]
    , grantees :: [(User, Amount)]
    }

data MetaData = MetaData
    { dataTime :: Maybe DateTime
    , location :: Maybe Text
    }

newtype User = User { name :: Text } deriving (Eq, Show)

data Amount = Amount
    { value :: Float
    , currency :: Currency
    } deriving (Eq, Show)

data Currency = EUR | CHF deriving (Eq, Show)

data DateTime = DateTime
    { date :: Date
    , time :: Time
    }

data Date = Date
    { day :: Int
    , month :: Int
    , year :: Int
    }

data Time = Time
    { hour :: Int
    , minutes :: Int
    }
