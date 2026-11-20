{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Example where

pick :: (t,[b]) -> [b]
pick (_,xs) = xs

-- Integral k does not occur in the result, so its choice cannot be inferred
-- from either list view. Numeric defaulting keeps the source accepted.
views :: forall a t. Integral a => a -> t -> Int -> ([Bool],[Char])
views _ extra _ =
    let go :: forall a. a -> ([Bool],[Char])
        go _ =
            let shared :: forall k b. Integral k => (t,[b])
                shared = (extra,[])
            in (pick shared, pick shared)
    in go True
