module Example where
tag 'a' = 1
tag _ = 2
named "ab" = 3
named _ = 4
pick c = if c == 'z' then 10 else 20
observed x = tag 'a' + named ('a':'b':[]) + pick 'z'
