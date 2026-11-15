module Example where
singleton x = [x]
flattened x = head (concatMap singleton (x:undefined))
