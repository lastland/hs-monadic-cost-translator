{-# LANGUAGE ScopedTypeVariables #-}
module Example where

make :: Foldable f => Int -> Maybe (f Int)
make n
    | n < 0 = error "negative count"
    | n == 0 = Nothing
    | otherwise = make (n - 1)

views :: Int -> ((Maybe [Int], Maybe [Int]), Maybe (Maybe Int))
views n =
    let shared :: forall f. Foldable f => Maybe (f Int)
        shared = make n
    in ((shared, shared), shared)
