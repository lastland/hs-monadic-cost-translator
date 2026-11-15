{-# LANGUAGE ScopedTypeVariables #-}
module Example where
caseAlias x = case x of y -> (x,y)
caseParenthesized x = case ((x)) of y -> (x,y)
caseAnnotated :: forall a. a -> (a,a)
caseAnnotated x = case (x :: a) of y -> (x,y)
caseAs x = case x of y@_ -> (x,y)
guardAlias x | y <- x = (x,y)
guardParenthesized x | y <- ((x)) = (x,y)
guardAnnotated :: forall a. a -> (a,a)
guardAnnotated x | y <- (x :: a) = (x,y)
guardAs x | y@_ <- x = (x,y)
