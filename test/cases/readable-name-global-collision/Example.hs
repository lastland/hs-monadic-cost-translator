module Example where
double x = x + x
observed x = let doubleM y = y + 1 in double (doubleM x)
