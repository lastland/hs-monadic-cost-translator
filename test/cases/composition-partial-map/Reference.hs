module Generated where

import Memorist.Memorist
import Memorist.Data.List (ListT(..))
import qualified Memorist.Example.List as ML

addM :: Num a => Th a -> Th a -> MemoristM a
addM ta tb = do
    tick
    a <- force ta
    b <- force tb
    pure (a + b)

twiceM :: Num a => Th a -> MemoristM a
twiceM tx = do
    tick
    x <- force tx
    pure (x + x)

observedM :: Num a => Th a -> MemoristM a
observedM tx = do
    tick
    -- (add . twice) x is one partial application, reused by both map calls.
    doubled <- lazy (twiceM tx)
    callback <- lazy (pure (addM doubled))
    end <- lazy (pure NilT)
    second <- lazy (pure (ConsT tx end))
    input <- lazy (pure (ConsT tx second))
    mapped <- lazy (ML.mapMM callback input)
    combine <- lazy (pure addM)
    zero <- lazy (pure 0)
    ML.foldrM combine zero mapped
