{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module ErasedDataDefault where

import qualified Memorist.Memorist as M

class Marks a where
    marks :: a -> [Bool]
    marks _ = []
    marks _ = [M.tval (undefined :: M.Th Bool)]

instance Marks Bool
