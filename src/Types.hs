module Types where

import Data.Text
import Data.DateTime

data Transaction = Transaction { source :: User
                               , metaData :: MetaData
                               , reason :: Text
                               , debtors :: [(User, Amount)]
                               , grantees :: [(User, Amount)]
                               }

data MetaData = MetaData { dateTime :: Maybe DateTime
                         , location :: Maybe Text
                         }

data User = User { name :: Text }

data Amount = Amount { value :: Int
                     , currency :: Currency
                     }

data Currency = EUR | CHF
