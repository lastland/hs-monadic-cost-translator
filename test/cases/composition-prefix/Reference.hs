module Generated where

import Memorist.Memorist
import Memorist.Data.List (ListT(..))
import qualified Memorist.Example.List as ML

twiceM :: Num a => Th a -> MemoristM a
twiceM tx = do
    tick
    x <- force tx
    pure (x + x)

observedM :: Num a => Th a -> MemoristM a
observedM tx = do
    tick
    let composed value = do
            middle <- lazy (twiceM value)
            twiceM middle
    callback <- lazy (pure composed)
    end <- lazy (pure NilT)
    input <- lazy (pure (ConsT tx end))
    mapped <- lazy (ML.mapMM callback input)
    ML.headM mapped
