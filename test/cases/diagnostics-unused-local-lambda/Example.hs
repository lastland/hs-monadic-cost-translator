{-# LANGUAGE BangPatterns #-}
module Example where
views :: t -> ()
views _ = let dead = \x -> x in ()
