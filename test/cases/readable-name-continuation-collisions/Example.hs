module Example where
choose 0 = 0
choose nextEquation0 = nextEquation0
observed nextGuard0 | nextGuard0 > 0 = choose nextGuard0 | otherwise = 0
