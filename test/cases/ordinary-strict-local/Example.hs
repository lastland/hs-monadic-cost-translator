{-# LANGUAGE BangPatterns #-}
module Example where
work :: Num a => a -> a
work n = n + n
views :: Num t => t -> ()
views input = let !keeper = work input in ()
