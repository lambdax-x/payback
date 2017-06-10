{-# LANGUAGE OverloadedStrings #-}
module Payback where

import qualified Data.HashMap as M
import Data.Function
import Data.List
import Types

type Debts = M.Map (User, User) [Amount]

updateDebts :: Debts -> Transaction -> Debts
updateDebts debts trans = foldl addDebt debts $ debtors trans
    where
        addDebt :: Debts -> (User, Amount) -> Debts
        addDebt currentDebts (usr, amt) =
            M.insertWith combine (usr, source trans) [amt] currentDebts

        combine :: [Amount] -> [Amount] -> [Amount]
        combine as [] = as
        combine new@[Amount nv nc] (a@(Amount ov oc) : as)
            | oc == nc = Amount { value = ov + nv, currency = nc} : as
            | otherwise = a : (combine new as)

computeDebts :: [Transaction] -> Debts
computeDebts = foldl updateDebts M.empty