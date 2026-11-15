module Example where
data Higher f a = Higher (f a)
wrap x = Higher x
first (Higher (x:_)) = x
observed x = first (wrap [x])
