{-# LANGUAGE ScopedTypeVariables #-}
module Example where

paid :: a -> a
paid x = x

partition :: forall a t. Integral a => a -> t -> Int -> Bool -> ((a,[Bool]), (a,[Char]))
partition seed _ count branch =
    let go :: forall t. t -> ((a,[Bool]), (a,[Char]))
        go _ =
            let shared :: forall key b. Integral key => ((key,[b]), a)
                shared = ((paid ((if count == 0 then 9 else 15) `quot` 3), []), seed)
            in case (shared :: ((a,[Bool]), a)) of
                (x,_) -> case (shared :: ((a,[Char]), a)) of (y,_) -> (x,y)
    in if branch then go True else go 'c'

views :: forall a. Integral a => a -> Int
      -> (((a,[Bool]), (a,[Char])), ((Integer,[Bool]), (Integer,[Char])))
views seed n = (partition seed () n True, partition (0 :: Integer) 't' n False)
