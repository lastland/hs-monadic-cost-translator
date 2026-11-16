{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module AnnotatedRecordPatterns where

import qualified Memorist.Memorist as M

recordPattern :: M.Annotated Int -> Int
recordPattern (M.Annotated { M.value = result }) = result

punPattern :: M.Annotated Int -> Int
punPattern (M.Annotated { M.value }) = value

wildcardPattern :: M.Annotated Int -> Int
wildcardPattern (M.Annotated {..}) = value

emptyPattern :: M.Annotated Int -> Int
emptyPattern (M.Annotated {}) = 3
