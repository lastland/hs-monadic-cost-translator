module Generated where

import Memorist.Memorist
import Memorist.Data.List (ListT(..))
import qualified Memorist.Example.List as ML

singletonM :: Th a -> MemoristM (ListT a)
singletonM tx = do
    tick
    end <- lazy (pure NilT)
    pure (ConsT tx end)

flattenedM :: Th a -> MemoristM a
flattenedM tx = do
    tick
    callback <- lazy (pure singletonM)
    bottomTail <- lazy (pure undefined)
    input <- lazy (pure (ConsT tx bottomTail))
    flattened <- lazy (ML.concatMapM callback input)
    ML.headM flattened
