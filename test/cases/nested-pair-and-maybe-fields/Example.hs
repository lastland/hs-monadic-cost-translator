module Example where
data Nested a = End a | More (Nested (a,a))
readNested (More (End (x,_))) = x
readMaybe (Just x) = x
nested x = readMaybe (Just (readNested (More (End (x,undefined)))))
