{-# LANGUAGE ScopedTypeVariables #-}
module Example where

select :: k -> j -> ((k,j), (t,[Bool])) -> ((k,j), (t,[Char])) -> (Int,Int)
select _ _ _ _ = (0,0)

-- Inference of one hidden owner is not evidence for a multi-variable key.
views :: forall a t. Integral a => a -> t -> Int -> (Int,Int)
views seed extra _ =
    let go :: forall a. a -> (Int,Int)
        go _ =
            let shared :: forall k j b. (Integral k, Integral j) => ((k,j), (t,[b]))
                shared = ((15 `quot` 3, 21 `quot` 3), (extra, []))
            in select seed seed shared shared
    in go True
