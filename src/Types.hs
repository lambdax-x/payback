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

data User = User { name :: Text }

data Amount = Amount
    { value :: Int
    , currency :: Currency
    }

data Currency = EUR | CHF

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
