{-# LANGUAGE MagicHash #-}
module Example where
data Box# a = Box# a
readBox# (Box# x) = x
observed x = readBox# (Box# x)
