{-# LANGUAGE ScopedTypeVariables #-}
module Example where

select :: k -> ((k, Int), (t, [Bool])) -> ((Int, Int), (t, [Char])) -> (Int, Int)
select _ ((_,x),_) ((_,y),_) = (x,y)

-- The two keys in one invocation can coincide at Int. Capturing t does not
-- justify assigning a separate native root to each syntactically distinct key.
views :: forall a t. Integral a => a -> t -> Int -> (Int, Int)
views seed extra count =
    let helper key =
            let shared :: forall k b. Integral k => ((k, Int), (t, [b]))
                shared = ((15 `quot` 3, count), (extra, []))
            in select key shared shared
    in helper seed
