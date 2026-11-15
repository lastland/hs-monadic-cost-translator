module Example where
keep x = let unused = if (undefined :: Bool) then x else x in x
