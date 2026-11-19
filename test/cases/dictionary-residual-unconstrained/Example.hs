{-# LANGUAGE ScopedTypeVariables #-}
module Example where
work :: Num a => a -> a
work n = n + n
views :: Int -> ((Int, [Bool]), (Int, [Char]))
views n =
    let shared :: forall a b. Num a => (a, [b])
        shared = (work (if n == 0 then 3 else 5), [])
    in (shared, shared)
