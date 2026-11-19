{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module AnnotatedPatternControl where

import qualified Memorist.Memorist as M

data Annotated a = Annotated { value :: a, annotation :: () }

extract :: Annotated Int -> Int
extract (Annotated result _) = result

recordPattern :: Annotated Int -> Int
recordPattern (Annotated { value = result }) = result

punPattern :: Annotated Int -> Int
punPattern (Annotated { value }) = value

wildcardPattern :: Annotated Int -> Int
wildcardPattern (Annotated {..}) = value

runtimeType :: M.Annotated Int -> M.Annotated Int
runtimeType = id
