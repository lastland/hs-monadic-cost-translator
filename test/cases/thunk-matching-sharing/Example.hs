{-# LANGUAGE ScopedTypeVariables #-}
module Example where
empty n = []
sharedEmpty = empty (1::Int)
polyCase u = case (sharedEmpty :: [Int]) of ys -> (sharedEmpty :: [Int],ys)
polyGuard u | ys <- (sharedEmpty :: [Bool]) = (sharedEmpty :: [Bool],ys)
localPolyCase u = let ys = empty u in case (ys :: [Int]) of ints -> case (ys :: [Bool]) of bools -> (ints,bools)
localPolyGuard u | let ys = empty u, ints <- (ys :: [Int]), bools <- (ys :: [Bool]) = (ints,bools)
work n = n+n
shared :: Num a => a
shared = work 5
sameCase u = case (shared :: Int) of y -> (shared :: Int,y)
sameGuard u | y <- (shared :: Int) = (shared :: Int,y)
different u | y <- (shared :: Integer) = (shared :: Int,y)
scopedCase :: forall a. Num a => a -> (a,a)
scopedCase x = case (shared :: a) of y -> (shared :: a,y)
scopedGuard :: forall a. Num a => a -> (a,a)
scopedGuard x | y <- (shared :: a) = (shared :: a,y)
localScoped :: forall a. Num a => a -> (a,a)
localScoped x = let go :: forall b. Num b => b -> (b,b); go y | z <- (shared :: b) = (shared :: b,z) in go x
wide :: Num a => (a,[b])
wide = (work 5,[])
wideBool u = case (wide :: (Int,[Bool])) of y -> (wide :: (Int,[Bool]),y)
wideInt u | y <- (wide :: (Int,[Int])) = (wide :: (Int,[Int]),y)
wideInteger u = case (wide :: (Integer,[Bool])) of y -> (wide :: (Integer,[Bool]),y)
failing :: Num a => a
failing = error "ignored shared value evaluated"
ignoredConcrete x = case (failing :: Int) of _ -> x
ignoredScoped :: forall a. Num a => a -> a
ignoredScoped x | _ <- (failing :: a) = x
ignoredDefault x | _ <- failing = x
ignoredLocalScoped x = let go :: forall a. Num a => a -> a; go y = case (failing :: a) of _ -> y in go x
defaultedCase x = case shared of y -> if y+y > 0 then x else x
defaultedGuard x | y <- shared, y+y > 0 = x | otherwise = x
bottomCase x = let y = work (undefined :: Int) in case y of z -> (x,z)
bottomGuard x | let y = work (undefined :: Int), z <- (y :: Int) = (x,z)
computedBottomCase x = case work (undefined :: Int) of z -> (x,z)
computedBottomGuard x | z <- work (undefined :: Int) = (x,z)
