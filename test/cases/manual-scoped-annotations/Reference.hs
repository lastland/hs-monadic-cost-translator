{-# LANGUAGE ScopedTypeVariables #-}
-- The reference captures the outer thunk directly; it never repeats a scoped
-- source annotation in the inner closure.
module Generated
    ( workM, ignoreM, caseParameterM, guardParameterM, caseParameterRenamedM
    , caseFactoryM, guardFactoryM, guardFactoryRenamedM
    ) where

import qualified Memorist.Memorist as M

data Bindings = Bindings { intCache :: M.Th Int, integerCache :: M.Th Integer }

large :: Num a => a
large = 18446744073709551616

initialize :: M.MemoristM Bindings
initialize = do
    int <- M.lazy (M.lazy (pure large) >>= workValue)
    integer <- M.lazy (M.lazy (pure large) >>= workValue)
    pure (Bindings int integer)

-- Keep all model cache allocations beneath a negative root, separate from the
-- caller's ordinary nonnegative names.
globalBindings :: Bindings
globalBindings = M.tval (M.value (M.eval (M.lazy initialize) (M.fromInt (-1))))

workValue :: (Num a, Eq a) => M.Th a -> M.MemoristM a
workValue argument = do
    M.tick
    value <- M.force argument
    if value == 0 then pure (value + 1) else do
        zero <- M.lazy $ do
            left <- M.force argument
            right <- M.force argument
            pure (left - right)
        result <- workValue zero
        pure (result + 1)

workM :: (Num a, Eq a) => M.Th a -> M.MemoristM a
workM = workValue

ignoreM :: M.Th a -> M.MemoristM Int
ignoreM _ = M.tick >> pure 1

caseParameterM, guardParameterM, caseParameterRenamedM
    :: M.Th a -> M.MemoristM Int
caseParameterM = parameter
guardParameterM = parameter
caseParameterRenamedM = parameter

parameter :: M.Th a -> M.MemoristM Int
parameter argument = do
    M.tick
    let inner :: M.Th Bool -> M.MemoristM Int
        inner _ = M.tick >> ignoreM argument
    true <- M.lazy (pure True)
    inner true

-- Only these two dictionaries are exercised. Their cached source work calls
-- are distinct: Int wraps the generalized literal to zero, Integer does not.
class (Num a, Eq a) => NumericCache a where
    sharedNumeric :: Bindings -> M.Th a

instance NumericCache Int where
    sharedNumeric = intCache

instance NumericCache Integer where
    sharedNumeric = integerCache

caseFactoryM, guardFactoryM, guardFactoryRenamedM
    :: NumericCache a => M.Th a -> M.MemoristM Int
caseFactoryM = factory globalBindings
guardFactoryM = factory globalBindings
guardFactoryRenamedM = factory globalBindings

factory :: forall a. NumericCache a => Bindings -> M.Th a -> M.MemoristM Int
factory bindings _ = do
    M.tick
    let captured = sharedNumeric bindings :: M.Th a
        inner :: M.Th Bool -> M.MemoristM Int
        inner _ = do
            M.tick
            value <- M.force captured
            pure (if value == 1 then 1 else 2)
    true <- M.lazy (pure True)
    inner true
