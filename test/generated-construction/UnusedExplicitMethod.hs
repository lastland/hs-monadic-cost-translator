{-# LANGUAGE TypeApplications #-}
module UnusedExplicitMethod where

import qualified Memorist.Memorist as M

unused :: M.MemoristM Int
unused =
    let hidden = pure @[] @Int 17
    in pure @M.MemoristM @Int 3
