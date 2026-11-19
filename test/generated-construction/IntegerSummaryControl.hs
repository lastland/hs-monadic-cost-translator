{-# LANGUAGE TypeApplications #-}
module IntegerSummaryControl where

import qualified Memorist.Memorist as M

equal :: Integer -> Integer -> Bool
equal = (==) @Integer

ordered :: Integer -> Integer -> Ordering
ordered = compare @Integer

arithmetic :: Integer -> Integer -> Integer
arithmetic left right = (left + right) * 2 - negate right

division :: Integer -> Integer -> (Integer, Integer)
division = quotRem @Integer

display :: Integer -> String
display = show @Integer

representation :: Integer -> M.MemoristM Integer
representation = M.reprM
