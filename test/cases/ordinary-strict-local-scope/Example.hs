{-# LANGUAGE BangPatterns #-}
module Example where

work :: Num t => t -> t
work input = input + input

views :: Num t => t -> ()
views input = () where !keeper = work input

skipped :: Num t => t -> ()
skipped input
  | False, let !keeper = work input = ()
  | otherwise = ()

failed :: Num t => t -> ()
failed input
  | let !keeper = work input, False = ()
  | otherwise = ()

dependent :: Num t => t -> ()
dependent input =
  let !keeper = case later of (value, _) -> value
      later = (work input, undefined :: Bool)
  in ()
