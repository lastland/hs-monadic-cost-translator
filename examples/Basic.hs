module Basic where

double :: Int -> Int
double x = x + x

shared x = let y = double x in y + y

captured x =
    let y = double x
        add z = y + z
    in add x + add x

keep x = let unused = x * (undefined :: Int) in x
