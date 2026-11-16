module Example where
work :: Int -> Int
work x = x+x
keep x = x
returnedShared x = let y = work x in (keep y,keep y)
alias x = (x,x)
caseAlias x = case x of y -> (x,y)
guardAlias x | y <- x = (x,y)
separate x = (work x,work x)
localPair x = let y = work x in (y,y)
caseLocal x = let y = work x in case y of z -> (y,z)
caseAnnotated x = let y = work x in case (y :: Int) of z -> (y,z)
guardLocal x | let y = work x, z <- y = (y,z)
guardAnnotated x | let y = work x, z <- (y :: Int) = (y,z)
computedCase x = case work x of y -> (y,y)
computedGuard x | y <- work x = (y,y)
pair a b = (a,b)
captured x xs = map (pair x) xs
computedCapture x xs = map (pair (work x)) xs
specialized x xs = map ((pair . work) x) xs
nestedSpecialized x xs = map (((pair . work) . work) x) xs
