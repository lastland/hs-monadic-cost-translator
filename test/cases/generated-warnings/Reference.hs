{-# LANGUAGE TypeFamilies #-}
module Generated (PairT(..), unboxM, capturedM, chooseM, composedM, singletonM, stringPatternM) where

import qualified Example as S
import qualified Memorist.Memorist as M
import Memorist.Data.List (ListT(..))

data PairT = PairT (M.Th Int) (M.Th Int)

instance M.Repr S.Pair where
    type Rep S.Pair = PairT
    reprM (S.Pair a b) = PairT <$> M.lazy (pure a) <*> M.lazy (pure b)
    abstract (PairT a b) = S.Pair (M.tval a) (M.tval b)

unboxM :: M.Th PairT -> M.MemoristM Int
unboxM pair = do
    M.tick
    p <- M.force pair
    case p of
        PairT x _ -> M.force x

capturedM :: M.Th Int -> M.MemoristM Int
capturedM x = do
    M.tick
    -- The captured y is allocated once, outside both calls of add.
    y <- M.lazy $ do
        a <- M.force x
        b <- M.force x
        pure (a + b)
    let add z = do
            M.tick
            a <- M.force y
            b <- M.force z
            pure (a + b)
    left <- M.lazy (add x)
    right <- M.lazy (add x)
    a <- M.force left
    b <- M.force right
    pure (a + b)

chooseM :: M.Th Int -> M.MemoristM Int
chooseM x = do
    M.tick
    a <- M.force x
    case a of
        0 -> pure 0
        _ -> M.force x

composedM :: M.Th Int -> M.MemoristM Int
composedM x = do
    M.tick
    inner <- M.lazy (capturedM x)
    capturedM inner

singletonM :: M.Th Int -> M.MemoristM (ListT Int)
singletonM x = do
    M.tick
    end <- M.lazy (pure NilT)
    pure (ConsT x end)

stringPatternM :: M.Th Int -> M.Th (ListT Char) -> M.MemoristM Int
stringPatternM field text = do
    M.tick
    list <- M.force text
    case list of
        NilT -> M.force field
        ConsT char rest -> do
            c <- M.force char
            if c == 'a' then do
                tailList <- M.force rest
                case tailList of
                    NilT -> M.force field
                    ConsT _ _ -> M.force field
            else M.force field
