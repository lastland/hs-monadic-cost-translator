module HigherKinded where

data Higher f a = Higher (f a)

data Fix f = In (f (Fix f))

data Nest f = Nest (Maybe (f (Nest f)))

data Layer a = Layer a

data Mixed f a = Leaf a | Branch (f (Mixed f [a]))

roll x = In x

isEmpty (In []) = True
isEmpty (In (_ : _)) = False

observed x = if isEmpty (roll []) then x else 0

wrap x = Higher [x]

firstListed (Higher (x : _)) = x

listed x = firstListed (wrap x)

forest n = In [forest (n + 1), forest (n + 2)]

nested n = Nest (Just [nested (n + 1)])

tagged x = Higher (Layer x)
