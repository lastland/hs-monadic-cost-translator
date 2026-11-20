module Example where
data Higher f a = Higher a (f a)
wrap x ys = Higher x ys
first (Higher _ (x:_)) = x
observed x = first (wrap undefined [x])
