{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module GenericDictionary where

import qualified Memorist.Memorist as M

genericComputation :: M.Memorist s m => M.Th Int -> m Int
genericComputation thunk = M.force thunk >>= pure

genericRepresentation :: M.Repr a => a -> M.MemoristM (M.Rep a)
genericRepresentation = M.reprM

genericScalar :: Num a => a -> a
genericScalar input = input + input

class FirstOrder a where
    firstOrder :: a -> a

genericHome :: FirstOrder a => a -> a
genericHome = firstOrder
