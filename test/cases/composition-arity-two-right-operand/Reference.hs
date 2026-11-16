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

observedM :: Num a => Th a -> MemoristM a
observedM tx = do
    tick
    -- (map . add) x captures the unary function add x, not an Int result.
    plus <- lazy (pure (addM tx))
    mapper <- lazy (pure (ML.mapMM plus))
    innerEnd <- lazy (pure NilT)
    inner <- lazy (pure (ConsT tx innerEnd))
    outerEnd <- lazy (pure NilT)
    outer <- lazy (pure (ConsT inner outerEnd))
    mapped <- lazy (ML.mapMM mapper outer)
    first <- lazy (ML.headM mapped)
    ML.headM first
