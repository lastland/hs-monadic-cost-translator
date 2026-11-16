module Generated where

import qualified Memorist.Memorist as M
import qualified Memorist.Data.List as L

namedM :: M.Th Int -> M.MemoristM Int
namedM x = do
    M.tick
    value <- M.force x
    pure (negate value)

largeM :: M.Th Integer -> M.MemoristM Integer
largeM x = do
    M.tick
    value <- M.force x
    pure (negate value)

negateM :: M.Th Int -> M.MemoristM Int
negateM x = M.tick >> ((+ 1) <$> M.force x)

shadowedM :: M.Th Int -> M.MemoristM Int
shadowedM x = M.tick >> negateM x

repeatedM :: M.Th Int -> M.MemoristM Int
repeatedM x = do
    M.tick
    value <- M.force x
    let result = negate value
    pure (result + result)

mappedM :: M.Th (L.ListT Int) -> M.MemoristM (L.ListT Int)
mappedM xs = M.tick >> mapNegated xs

mapNegated :: M.Th (L.ListT Int) -> M.MemoristM (L.ListT Int)
mapNegated xs = do
    M.tick
    values <- M.force xs
    case values of
        L.NilT -> pure L.NilT
        L.ConsT x rest -> do
            y <- M.lazy (negate <$> M.force x)
            ys <- M.lazy (mapNegated rest)
            pure (L.ConsT y ys)

doubleM :: M.Th Int -> M.MemoristM Int
doubleM x = do
    M.tick
    value <- M.force x
    pure (value + value)

composedM :: M.Th Int -> M.MemoristM Int
composedM x = M.tick >> (negate <$> doubleM x)

keepM :: M.Th a -> M.Th b -> M.MemoristM a
keepM x _ = M.tick >> M.force x

ignoredM :: M.Th Int -> M.MemoristM Int
ignoredM x = do
    M.tick
    unused <- M.lazy (error "unused negation operand")
    keepM x unused
