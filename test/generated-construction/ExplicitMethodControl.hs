{-# LANGUAGE ImpredicativeTypes, RankNTypes, TypeApplications #-}
module ExplicitMethodControl where

import qualified Memorist.Memorist as M
import StructuralViewControl (preserve)

ordinary :: M.MemoristM Int
ordinary = pure @M.MemoristM @Int 17

allocated :: M.MemoristM (M.Th (M.MaybeT Int))
allocated = do
    root <- M.lazy @M.MemoristM @(forall a. M.MaybeT a)
        (pure @M.MemoristM @(forall a. M.MaybeT a) M.NothingT)
    pure (preserve @(forall a. M.MaybeT a) (\payload -> payload) root)
