module Example where
work n = n+n
shared :: Num a => a
shared = work 5
same u = (shared :: Int, shared :: Int)
different u = (shared :: Int, shared :: Integer)
local x = let y = work x in (y,y)
