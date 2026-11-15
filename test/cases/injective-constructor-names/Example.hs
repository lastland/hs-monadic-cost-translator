module Example where
data R a = a :%: a | Constructor_58_37_58 a
pick (x :%: _) = x
pick (Constructor_58_37_58 x) = x+x
observed x = pick (x :%: undefined) + pick (Constructor_58_37_58 x)
