module Main where

import System.Environment
import qualified Data.ByteString.Lazy as B
import Control.Monad ((>=>))
import Data.Aeson (decode)
import Payback.Types
import Payback (computeDebts)

decodeTxs :: B.ByteString -> Maybe [Transaction]
decodeTxs = decode >=> sequence

main :: IO ()
main = decodeTxs <$> B.getContents
    >>= \mtxs -> return (case mtxs of
                           Nothing -> "invalid input"
                           Just txs -> show . computeDebts $ txs)
    >>= print

