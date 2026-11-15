module Example where
work n = n+1
shared :: Foldable t => (Int, t Int)
shared = (work 5, undefined)
asList x = case (shared :: (Int,[Int])) of { (n,_) -> n+x }
asMaybe x = case (shared :: (Int,Maybe Int)) of { (n,_) -> n+x }
