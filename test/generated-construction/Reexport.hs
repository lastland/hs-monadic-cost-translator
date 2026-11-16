module Reexport where

import qualified ReasoningBridge as Innocent

extract :: Innocent.Annotated Int -> Int
extract = Innocent.value
