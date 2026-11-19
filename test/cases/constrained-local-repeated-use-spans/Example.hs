{-# LANGUAGE ScopedTypeVariables #-}
module Example where
work :: Num a => a -> a
work n = n + n
views :: Int -> (Maybe Int, Maybe Int)
views n =
    let shared :: forall a. Num a => Maybe a
        shared = Just (work (if n == 0 then 3 else 5))
    in (
{-# LINE 100 "RepeatedUse.hs" #-}
        shared,
{-# LINE 100 "RepeatedUse.hs" #-}
        shared)
