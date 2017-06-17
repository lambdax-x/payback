module Main where

import System.Environment
import qualified Data.Text.IO as T
import Control.Monad ((>=>))
import Data.Aeson (decode)
import Payback.Types
import Payback (computeDebts, debtsToCSV)
import Payback.Parse (parsePaylog)
import Text.Megaparsec

main :: IO ()
main = parse parsePaylog "stdin" <$> T.getContents
   >>= mapM_ putStrLn . processResult
    where processResult (Left err) = [parseErrorPretty err]
          processResult (Right txs) = debtsToCSV $ computeDebts txs