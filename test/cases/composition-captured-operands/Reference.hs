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
    -- These are distinct source expressions. Share each across map calls,
    -- but do not merge the two occurrences of twice x with one another.
    leftOperand <- lazy (twiceM tx)
    rightInner <- lazy (twiceM tx)
    rightOperand <- lazy (twiceM rightInner)
    let composed value = do
            middle <- lazy (addM rightOperand value)
            addM leftOperand middle
    callback <- lazy (pure composed)
    end <- lazy (pure NilT)
    second <- lazy (pure (ConsT tx end))
    input <- lazy (pure (ConsT tx second))
    mapped <- lazy (ML.mapMM callback input)
    combine <- lazy (pure addM)
    zero <- lazy (pure 0)
    ML.foldrM combine zero mapped
