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

foldedM :: Num a => Th a -> MemoristM a
foldedM tx = do
    tick
    callback <- lazy (pure (add4M tx tx))
    zero <- lazy (pure 0)
    end <- lazy (pure NilT)
    second <- lazy (pure (ConsT tx end))
    input <- lazy (pure (ConsT tx second))
    ML.foldrM callback zero input
