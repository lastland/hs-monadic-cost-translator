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

foldedM :: Num a => Th a -> MemoristM a
foldedM tx = do
    tick
    callback <- lazy (pure addM)
    zero <- lazy (pure 0)
    end <- lazy (pure NilT)
    second <- lazy (pure (ConsT tx end))
    input <- lazy (pure (ConsT tx second))
    ML.foldlM callback zero input
