module Example where
keep x = let unused = error "unused" in if True || undefined then x else unused
