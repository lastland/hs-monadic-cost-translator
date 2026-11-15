module Example where
data Box = Box Int
data Record = Record { first :: Int, second :: Int }
work :: Int -> Int
work n = n+n
bad :: a -> a
bad x = error "demanded bad computation"
caseComputed x = case work x of y -> (y,y)
guardComputed x | y <- work x = (y,y)
caseConditional, guardConditional, caseBad, guardBad, sourceLet :: Int -> (Int,Int)
caseConditional x = case (if (undefined :: Bool) then x else x) of y -> (y,y)
guardConditional x | y <- (if (undefined :: Bool) then x else x) = (y,y)
caseBad x = case bad x of y -> (y,y)
guardBad x | y <- bad x = (y,y)
sourceLet x = let y = x in (y,y)
caseVariable, caseWildcard, guardVariable, guardWildcard, caseLiteral, guardLiteral :: Int -> Int
caseVariable x = case x of y -> 7
caseWildcard x = case x of _ -> 7
guardVariable x | y <- x = 7
guardWildcard x | _ <- x = 7
caseLiteral x = case x of { 0 -> 7; _ -> 8 }
guardLiteral x | 0 <- x = 7 | otherwise = 8
caseConstructor, guardConstructor :: Box -> Int
caseConstructor x = case x of Box _ -> 7
guardConstructor x | Box _ <- x = 7
caseAs, guardAs :: Box -> (Box,Box)
caseAs x = case x of y@(Box _) -> (x,y)
guardAs x | y@(Box _) <- x = (x,y)
caseNested, guardNested :: (Box,Box) -> Int
caseNested x = case x of { (Box 0, Box 0) -> 1; _ -> 2 }
guardNested x | (Box 0, Box 0) <- x = 1 | otherwise = 2
caseRecord, guardRecord :: Record -> Int
caseRecord x = case x of { Record { second = 0, first = 0 } -> 1; _ -> 2 }
guardRecord x | Record { second = 0, first = 0 } <- x = 1 | otherwise = 2
caseGuardSequence, guardSequence :: Int -> Int
caseGuardSequence x = case x of { y | y > 0, 0 <- bad y -> 1; _ -> 2 }
guardSequence x | x > 0, 0 <- bad x = 1 | otherwise = 2
ignore :: Int -> Int -> Int
ignore unused x = x
conditionalArg, nestedLetArg, scalar, scalarWork, primitiveWork, conditionalWork :: Int -> Int
conditionalArg x = ignore (if (undefined :: Bool) then x else x) x
nestedLetArg x = ignore (let y = bad x in y+y) x
scalar x = x
scalarWork x = scalar (work x)
primitiveWork x = work x + work (x+1)
conditionalWork x = choose (positive x) (work x) (bad x)
add :: Int -> Int -> Int
add x y = x+y
choose :: Bool -> Int -> Int -> Int
choose c x y = if c then x else y
positive :: Int -> Bool
positive x = x > 0
boolAnd, boolOr :: Bool -> Bool -> Bool
boolAnd x y = x && y
boolOr x y = x || y
shortAnd, shortOr :: Int -> Bool
shortAnd x = boolAnd (positive x) (positive (work x))
shortOr x = boolOr (positive x) (positive (work x))
