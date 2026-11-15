module Example where
add3 a b c = a+b+c
double x = x+x
composed x = (add3 x . double) x x
