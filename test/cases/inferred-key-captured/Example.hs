{-# LANGUAGE ScopedTypeVariables #-}
module Example where

select :: k -> ((k, Int), (t, [Bool])) -> ((k, Int), (t, [Char])) -> (Int, Int)
select _ ((_,x),_) ((_,y),_) = (x,y)

views :: forall a t. Integral a => a -> t -> Int -> (Int,Int)
views seed extra count =
    let go :: forall a. a -> (Int,Int)
        go _ =
            let shared :: forall k b. Integral k => ((k,Int),(t,[b]))
                shared = ((15 `quot` 3,count),(extra,[]))
            in select seed shared shared
    in if count == 0 then go True else go 'c'
