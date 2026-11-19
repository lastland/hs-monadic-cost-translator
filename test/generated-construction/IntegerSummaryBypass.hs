{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module IntegerSummaryBypass where

import qualified Memorist.Memorist as M

abstraction :: Integer -> Integer
abstraction = M.abstract

payload :: M.Th Integer -> Integer
payload = M.tval

protected :: M.Th Integer -> Bool
protected input = input == input

generic :: Eq a => a -> a -> Bool
generic = (==)

wrongCarrier :: M.Annotated Integer
wrongCarrier = pure 17
