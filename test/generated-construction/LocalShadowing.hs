{-# OPTIONS_GHC -Wno-name-shadowing #-}
module LocalShadowing where

import Memorist.Base.Core

-- These binders are user functions, not the imported runtime definitions.
ordinary :: Int -> Int
ordinary number = let value = id; eval1 = id in value (eval1 number)

parameters :: (Int -> Int) -> (Int -> Int) -> Int -> Int
parameters value eval1 number = value (eval1 number)

-- An import alone does not use a reasoning operation.
annotation :: Annotated Int -> Annotated Int
annotation = id
