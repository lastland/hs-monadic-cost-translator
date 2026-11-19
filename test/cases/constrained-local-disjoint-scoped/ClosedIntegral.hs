{-# LANGUAGE ScopedTypeVariables #-}
module Example where

work :: Num t => t -> t
work n = n + n

views :: forall a. Integral a => a -> Int
      -> (((a, Int), (a, Int)), ((a, Integer), (a, Integer)))
views _ count =
    let shared :: forall key b. Integral key => ((a, key), [b])
        shared = ((work (if count < 0 then 3 else 5),
                   work ((if count == 0 then 33 else 21) `quot` 3)), [])
    in case (shared :: ((a, Int), [Bool])) of
        (intB, _) -> case (shared :: ((a, Int), [Char])) of
            (intC, _) -> case (shared :: ((a, Integer), [Bool])) of
                (integerB, _) -> case (shared :: ((a, Integer), [Char])) of
                    (integerC, _) -> ((intB, intC), (integerB, integerC))
