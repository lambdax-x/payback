module Main where

import System.Environment
import qualified Data.ByteString.Lazy as B
import Control.Monad ((>=>))
import Data.Aeson (decode)
import Types
import Payback (computeDebts, debtsToCSV)

decodeTxs :: B.ByteString -> Maybe [Transaction]
decodeTxs = decode >=> sequence

main :: IO ()
main = decodeTxs <$> B.getContents
    >>= \mtxs -> mapM_ putStrLn (case mtxs of
                                   Nothing -> ["invalid input"]
                                   Just txs -> debtsToCSV. computeDebts $ txs)
