{-# LANGUAGE BangPatterns #-}
module Example where

work :: t -> (t, Bool)
work input = (input, undefined)

views :: t -> ()
views input = let !keeper = work input in ()

aliases :: t -> ((t, Bool), (t, Bool))
aliases input = (keeper, keeper)
  where !keeper = work input
