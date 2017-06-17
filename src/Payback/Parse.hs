{-# OverloadedStrings #-}
module Payback.Parse (
    parseTransaction,
    parsePaylog
) where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.ByteString
import qualified Text.Megaparsec.Lexer as L
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Char
import Payback.Types

-- Common parsers
spaces = tab <|> char ' '
int = fromInteger <$> L.integer
symbol = L.symbol $ skipMany spaces
line :: Parser a -> Parser a
line p = p <* manyTill spaces (void eol <|> eof)

-- Payback Parsers
parseTime :: Parser Time
parseTime = mkTime <$> int <* char ':' <*> int

parseDate :: Parser Date
parseDate = mkDate <$> int <* char '-' <*> int <* char '-' <*> int

parseDateTime :: Parser DateTime
parseDateTime = between
    (char '[') (char ']')
    (mkDateTime <$> parseDate <* char ' ' <*> parseTime)

parseLocation :: Parser T.Text
parseLocation = T.pack <$> (symbol "at" >> some printChar)

parseReason :: Parser T.Text
parseReason = T.pack <$> (symbol "for" >> some printChar)

parseUser :: Parser User
parseUser = User . T.pack <$> some name
    where name = satisfy (\c -> isPrint c && not (isNumber c)) <?> "name"

parseAmount :: Parser Amount
parseAmount = mkAmount <$> parseValue <*> parseCurrency
    where parseValue = realToFrac <$> L.number
          parseCurrency = (symbol "€" >> return EUR)
                       <|> (symbol "CHF" >> return CHF)

data TransactionComponentKind = Debt | Gift deriving (Show, Eq)

parseTransactionComponent :: Parser (TransactionComponentKind, User, Amount)
parseTransactionComponent = (,,) <$> (parseDebt <|> parseGift) <*> parseUser <*> parseAmount
    where parseDebt = symbol "-" >> return Debt
          parseGift = symbol "+" >> return Gift

unordered :: Parser a -> Parser b -> Parser (a, b)
unordered pa pb = try ((,) <$> pa <*> pb)
              <|> flip (,) <$> pb <*> pa

parseTransaction :: Parser Transaction
parseTransaction = do
    (datetime, source) <- line parseHeader
    (location, reason) <- unordered (optional $ line parseLocation)
                                    (optional $ line parseReason)
    components <- some (line parseTransactionComponent)

    return Transaction {
        source = source,
        reason = reason,
        metadata = Metadata {
            datetime = datetime,
            location = location
        },
        debtors = getComponentsOf Debt components,
        grantees = getComponentsOf Gift components
    }

    where
        parseHeader  = (,) <$> parseDateTime <* skipMany spaces <*> parseUser
        getComponentsOf kind = map (\(_,u,a) -> (u, a)) . filter (\(k,_,_) -> k == kind)

parsePaylog :: Parser [Transaction]
parsePaylog = some (parseTransaction <* space)