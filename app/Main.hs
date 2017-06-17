module Main where

import System.Environment
import qualified Data.ByteString as B
import Control.Monad ((>=>))
import Data.Aeson (decode)
import Payback.Types
import Payback (computeDebts, debtsToCSV)
import Payback.Parse (parsePaylog)
import Text.Megaparsec

decodeTxs :: B.ByteString -> Maybe [Transaction]
decodeTxs = decode >=> sequence

main :: IO ()
main = parse parsePaylog "stdin" <$> B.getContents
   >>= mapM_ putStrLn . processResult
    where processResult (Left err) = [parseErrorPretty err]
          processResult (Right txs) = debtsToCSV $ computeDebts txs