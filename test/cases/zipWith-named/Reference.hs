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

zippedM :: Num a => Th a -> MemoristM a
zippedM tx = do
    tick
    callback <- lazy (pure addM)
    leftBottom <- lazy (pure undefined)
    left <- lazy (pure (ConsT tx leftBottom))
    rightBottom <- lazy (pure undefined)
    right <- lazy (pure (ConsT tx rightBottom))
    zipped <- lazy (ML.zipWithM callback left right)
    ML.headM zipped
