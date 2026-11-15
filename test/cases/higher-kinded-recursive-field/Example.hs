module Example where
data Fix f = In (f (Fix f))
roll x = In x
isEmpty (In []) = True
isEmpty (In (_:_)) = False
observed x = if isEmpty (roll []) then x else 0
