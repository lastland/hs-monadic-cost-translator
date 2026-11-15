module Example where
single x = [x]
flat xs = concatMap single xs
observed xs = head (flat xs)
