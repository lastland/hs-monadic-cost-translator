module Generated where

import Memorist.Memorist
import Memorist.Data.List (ListT(..))
import qualified Memorist.Example.List as ML

add4M :: Num a => Th a -> Th a -> Th a -> Th a -> MemoristM a
add4M ta tb tc td = do
    tick
    a <- force ta
    b <- force tb
    c <- force tc
    d <- force td
    pure (a + b + c + d)

twiceM :: Num a => Th a -> MemoristM a
twiceM tx = do
    tick
    x <- force tx
    pure (x + x)

observedM :: Num a => Th a -> MemoristM a
observedM tx = do
    tick
    -- The first two supplied arguments leave a binary callback. Its doubled
    -- operand belongs to the partial application, not to a fold iteration.
    doubled <- lazy (twiceM tx)
    callback <- lazy (pure (add4M doubled tx))
    zero <- lazy (pure 0)
    end <- lazy (pure NilT)
    second <- lazy (pure (ConsT tx end))
    input <- lazy (pure (ConsT tx second))
    ML.foldrM callback zero input
