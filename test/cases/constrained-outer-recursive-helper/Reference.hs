{-# LANGUAGE ScopedTypeVariables #-}
module Generated (Scalar, oneM, oneLoopM, twiceM, cachedM, framesM, separateM, brokenHeadM, brokenRootM) where

import qualified Memorist.Data.List as L
import qualified Memorist.Example.List as ML
import qualified Memorist.Memorist as M

class Scalar a
instance Scalar Int
instance Scalar Integer

type V a = M.MaybeT (L.ListT a)
type P a = M.PairT (V a) (V a)
type Q a = M.PairT (P a) (P a)

expensive :: (Eq a, Num a) => M.Th a -> M.MemoristM a
expensive input = do
    M.tick
    n <- M.force input
    if n == 0 then pure (n + 1) else do
        next <- M.lazy (pure (n - n))
        value <- expensive next
        pure (value + 1)

body :: (Eq a, Num a) => M.MemoristM (V a)
body = do
    list <- M.lazy $ do
        first <- M.lazy (M.lazy (pure 5) >>= expensive)
        rest <- M.lazy (pure L.NilT)
        pure (L.ConsT first rest)
    pure (M.JustT list)

oneM :: forall a. (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (Q a)
oneM input count = do
    M.tick
    shared <- M.lazy body
    -- Every helper entry pays a tick; both loops retain the same outer producer.
    let helper :: M.Th a -> M.Th Int -> M.MemoristM (P a)
        helper argument remaining = do
            M.tick
            k <- M.force remaining
            if k == 0 then pure (M.PairT shared shared) else do
                next <- M.lazy (pure (k - 1))
                helper argument next
    left <- M.lazy $ do
        zipped <- M.lazy $ do
            callback <- M.lazy (pure helper)
            inputs <- M.lazy $ do
                rest <- M.lazy (pure L.NilT)
                pure (L.ConsT input rest)
            counts <- M.lazy $ do
                rest <- M.lazy (pure L.NilT)
                pure (L.ConsT count rest)
            ML.zipWithM callback inputs counts
        ML.headM zipped
    right <- M.lazy (helper input count)
    pure (M.PairT left right)

loop :: M.Th (V a) -> M.Th (V a) -> M.Th a -> M.Th Int -> M.MemoristM (P a)
loop left right input count = do
    M.tick
    k <- M.force count
    if k == 0 then pure (M.PairT left right) else do
        next <- M.lazy (pure (k - 1))
        loop left right input next

oneLoopM :: (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (P a)
oneLoopM input count = do
    M.tick
    shared <- M.lazy body
    loop shared shared input count

twiceM :: (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (Q a)
twiceM input count = do
    M.tick
    shared <- M.lazy body
    left <- M.lazy (loop shared shared input count)
    right <- M.lazy (loop shared shared input count)
    pure (M.PairT left right)

cachedM :: (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (Q a)
cachedM input count = do
    M.tick
    shared <- M.lazy body
    result <- M.lazy (loop shared shared input count)
    pure (M.PairT result result)

framesM :: (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (L.ListT (P a))
framesM input count = do
    M.tick
    shared <- M.lazy body
    let walk argument = do
            M.tick
            k <- M.force argument
            result <- M.lazy (pure (M.PairT shared shared))
            rest <- M.lazy (if k == 0 then pure L.NilT else do
                next <- M.lazy (pure (k - 1))
                walk next)
            pure (L.ConsT result rest)
    -- The source input is deliberately unused, including at every self-call.
    let _ = input
    walk count

separateM :: (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (Q a)
separateM input count = do
    M.tick
    leftShared <- M.lazy body
    rightShared <- M.lazy body
    left <- M.lazy (loop leftShared rightShared input count)
    right <- M.lazy (loop leftShared rightShared input count)
    pure (M.PairT left right)

brokenHeadM :: (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (Q a)
brokenHeadM input count = do
    M.tick
    shared <- M.lazy $ do
        list <- M.lazy $ do
            first <- M.lazy (error "recursive head failure")
            rest <- M.lazy (pure L.NilT)
            pure (L.ConsT first rest)
        pure (M.JustT list)
    left <- M.lazy (loop shared shared input count)
    right <- M.lazy (loop shared shared input count)
    pure (M.PairT left right)

brokenRootM :: (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (Q a)
brokenRootM input count = do
    M.tick
    shared <- M.lazy (error "recursive root failure")
    left <- M.lazy (loop shared shared input count)
    right <- M.lazy (loop shared shared input count)
    pure (M.PairT left right)
