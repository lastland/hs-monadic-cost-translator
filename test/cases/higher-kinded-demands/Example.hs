{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, TypeOperators #-}
module Example where
data Higher f a = Higher (f a)
data Fix f = In (f (Fix f))
data Nest f = Nest (Maybe (f (Nest f)))
data Layer a = Layer a
data Mixed f a = Leaf a | Branch (f (Mixed f [a]))
data Index = Key | Index :#: Index
data Tagged (i :: Index) = Tagged Int
data Tag (i :: k) = Tag Int
data Numeric f = Numeric (f 0)
data Named f = Named (f "key")
data Character f = Character (f 'x')
data Selected f = Selected (f 'Key)
data Operated f = Operated (f ('Key ':#: 'Key))
readNumeric (Numeric (Tag x)) = x
readNamed (Named (Tag x)) = x
readCharacter (Character (Tag x)) = x
readSelected (Selected (Tag x)) = x
readOperated (Operated (Tag x)) = x
indexKinds x = readNumeric (Numeric (Tag x)) + readNamed (Named (Tag x)) + readCharacter (Character (Tag x)) + readSelected (Selected (Tag x)) + readOperated (Operated (Tag x))
forest n = In [forest (n+1), forest (n+2)]
nested n = Nest (Just [nested (n+1)])
tagged x = Higher (Tagged x)
