{-# LANGUAGE ScopedTypeVariables #-}

-- Both source variants differ only in bound type-variable spelling. This
-- reference instead captures the outer value directly in the local closure.
module Generated
    ( workM, ignoreM, caseParameterM, guardParameterM, caseFactoryM, guardFactoryM
    ) where

import qualified Memorist.Memorist as M

data Bindings = Bindings
    { intCache :: M.Th Int
    , integerCache :: M.Th Integer
    }

initialize :: M.MemoristM Bindings
initialize = do
    int <- M.lazy $ M.lazy (pure (fromInteger 18446744073709551616 :: Int)) >>= workValue
    integer <- M.lazy $ M.lazy (pure (18446744073709551616 :: Integer)) >>= workValue
    pure (Bindings int integer)

-- Keep the source-sharing model outside the caller's ordinary allocator. The
-- enclosing suspension keeps every cached name beneath the negative root.
globalBindings :: Bindings
globalBindings = M.tval (M.value (M.eval (M.lazy initialize) (M.fromInt (-1))))

workValue :: (Num a, Eq a) => M.Th a -> M.MemoristM a
workValue argument = do
    M.tick
    value <- M.force argument
    if value == 0
        then pure (value + 1)
        else do
            difference <- M.lazy $ do
                left <- M.force argument
                right <- M.force argument
                pure (left - right)
            recursive <- workValue difference
            pure (recursive + 1)

workM :: (Num a, Eq a) => M.Th a -> M.MemoristM a
workM = workValue

ignoreM :: M.Th a -> M.MemoristM Int
ignoreM _ = M.tick >> pure 1

caseParameterM :: M.Th a -> M.MemoristM Int
caseParameterM argument = do
    M.tick
    let inner _ = do
            M.tick
            ignoreM argument
    true <- M.lazy (pure True)
    inner true

guardParameterM :: M.Th a -> M.MemoristM Int
guardParameterM argument = do
    M.tick
    let inner _ = do
            M.tick
            ignoreM argument
    true <- M.lazy (pure True)
    inner true

-- Explicit dictionary specialization keeps Int's overflowing literal separate
-- from Integer's nonzero literal, while repeated calls reuse each own thunk.
class (Num a, Eq a) => NumericCache a where
    sharedNumeric :: Bindings -> M.Th a

instance NumericCache Int where
    sharedNumeric = intCache

instance NumericCache Integer where
    sharedNumeric = integerCache

caseFactoryM :: NumericCache a => M.Th a -> M.MemoristM Int
caseFactoryM = caseFactoryWith globalBindings

caseFactoryWith :: forall a. NumericCache a => Bindings -> M.Th a -> M.MemoristM Int
caseFactoryWith bindings _ = do
    M.tick
    let captured = sharedNumeric bindings :: M.Th a
        inner _ = do
            M.tick
            value <- M.force captured
            if value == 1 then pure 1 else pure 2
    true <- M.lazy (pure True)
    inner true

guardFactoryM :: NumericCache a => M.Th a -> M.MemoristM Int
guardFactoryM = guardFactoryWith globalBindings

guardFactoryWith :: forall a. NumericCache a => Bindings -> M.Th a -> M.MemoristM Int
guardFactoryWith bindings _ = do
    M.tick
    let captured = sharedNumeric bindings :: M.Th a
        inner _ = do
            M.tick
            value <- M.force captured
            if value == 1 then pure 1 else pure 2
    true <- M.lazy (pure True)
    inner true
