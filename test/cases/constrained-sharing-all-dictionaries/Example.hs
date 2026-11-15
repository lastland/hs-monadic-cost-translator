module Example where
work n = n+n
shared :: (Num a, Num b) => (a,b)
shared = (work 5, work 5)
one x = case (shared :: (Int,Integer)) of { (n,_) -> n+x }
two x = case (shared :: (Int,Int)) of { (n,_) -> n+x }
