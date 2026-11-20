{-# LANGUAGE ScopedTypeVariables #-}
module ActionFieldEvidence where

import BoxedActionControl (Jobs(..), runFirst)
import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

number :: forall a. Eq a => a -> Q.Property
number input =
    let jobs = Jobs {spare = pure False, flag = True, first = pure () >>= \() -> (input == input) `seq` pure True}
    in Q.property (M.value (M.eval1 (runFirst jobs jobs)))
