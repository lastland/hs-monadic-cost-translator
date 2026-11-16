module LetWrapper where

import qualified Memorist.Memorist as M

extract :: M.Annotated Int -> Int
extract annotated = let wrapped = M.value in wrapped annotated
