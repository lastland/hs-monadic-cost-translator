module Records where

data Box = Box { payload :: Int, ignored :: Int }

readBox Box { payload = x } = x

recorded x = let b = Box { ignored = undefined, payload = x } in readBox b + payload b

data Pair a = a :*: a

first (x :*: _) = x

paired x = first (x :*: undefined)

data Nested a = End a | More (Nested (a, a))

readPair (More (End (x, _))) = x

readMaybe (Just x) = x

nested x = readMaybe (Just (readPair (More (End (x, undefined)))))
