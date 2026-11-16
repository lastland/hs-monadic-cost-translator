-- Handwritten source translation. Function entries tick; adapters do not.
module Generated where

import Memorist.Memorist
import Memorist.Data.List (ListT(..))
import qualified Memorist.Example.List as ML

add3M :: Num a => Th a -> Th a -> Th a -> MemoristM a
add3M ta tb tc = do
    tick
    a <- force ta
    b <- force tb
    c <- force tc
    pure (a + b + c)

twiceM :: Num a => Th a -> MemoristM a
twiceM tx = do
    tick
    x <- force tx
    pure (x + x)

partialM :: Num a => Th a -> MemoristM a
partialM tx = do
    tick
    let captured tz = do
            tick
            x <- force tx
            z <- force tz
            pure (x + z)
        composed ty = do
            doubled <- lazy (twiceM ty)
            added <- lazy (add3M tx tx doubled)
            captured added
    callback <- lazy (pure composed)
    bottomTail <- lazy (pure undefined)
    input <- lazy (pure (ConsT tx bottomTail))
    mapped <- lazy (ML.mapMM callback input)
    ML.headM mapped
