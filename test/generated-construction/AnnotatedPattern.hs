module AnnotatedPattern where

import qualified Memorist.Memorist as M

extract :: M.Annotated Int -> Int
extract (M.Annotated result _) = result

ordinary :: Int
ordinary =
    let _extract :: M.Annotated Int -> Int
        _extract (M.Annotated result _) = result
    in 1
