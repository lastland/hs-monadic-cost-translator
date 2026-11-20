{-# LANGUAGE ScopedTypeVariables #-}
module Example where

tie :: a -> ((a, [Bool]), (a, [Char])) -> ((a, [Bool]), (a, [Char]))
tie _ copies = copies

paid :: a -> a
paid x = x

views :: forall b. Integral b => b -> Int
      -> (((b, [Bool]), (b, [Char])), ((Integer, [Bool]), (Integer, [Char])))
views seed count =
    let helper key =
            let shared :: forall key b. Integral key => (key, [b])
                shared = (paid ((if count == 0 then 9 else 15) `quot` 3), [])
            in tie key (shared, shared)
    in (helper (seed :: b), helper (0 :: Integer))
