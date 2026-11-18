{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, RankNTypes, BangPatterns #-}
module Example where
work :: Num a => a -> a
work n = n+n
gate :: Num t => t -> Bool
gate _ = True
views :: forall t. Num t => t -> ()
views input =
  let shared :: forall b c d. (Num b,Num c,Num d) => (b,(c,d))
      shared = if gate (input :: t) then (work 5,(work 7,work 9)) else error "unused"
      helper :: Num Bool => () -> ((Bool,(Int,Int)),(Bool,(Int,Int)))
      helper _ = (shared,shared)
      keeper :: forall z. Num Bool => (((Bool,(Int,Int)),(Bool,(Int,Int))),[z])
      !keeper = (helper (),[])
  in ()
