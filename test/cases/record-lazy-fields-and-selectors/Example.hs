module Example where
data Box = Box { payload :: Int, ignored :: Int }
readBox Box { payload = x } = x
recorded x = let b = Box { ignored = undefined, payload = x } in readBox b + payload b
