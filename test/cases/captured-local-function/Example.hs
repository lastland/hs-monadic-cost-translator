module Example where
double x = x + x
captured x = let y = double x; add z = y + z in add x + add x
