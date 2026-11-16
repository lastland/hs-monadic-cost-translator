module Generated where

import Memorist.Memorist
import Memorist.Data.List (ListT(..))
import qualified Memorist.Example.List as ML
import qualified Memorist.Example.ListInf as MI

addM :: Num a => Th a -> Th a -> MemoristM a
addM ta tb = do
    tick
    a <- force ta
    b <- force tb
    pure (a + b)

badM :: Th a -> MemoristM (ListT a)
badM tn = do
    tick
    if (undefined :: Bool) then do
        end <- lazy (pure NilT)
        pure (ConsT tn end)
    else pure NilT

scannedM :: Num a => Th a -> MemoristM a
scannedM tx = do
    tick
    callback <- lazy (pure addM)
    input <- lazy (badM tx)
    scanned <- lazy (MI.scanlM callback tx input)
    ML.headM scanned
