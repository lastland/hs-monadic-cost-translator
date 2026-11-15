{-# LANGUAGE TypeOperators #-}
module Example where
data a :%: b = Pair a b
data Constructor_58_37_58 a = Plain a
data SharingStep = SourceStep Int
pickPair (Pair x _) = x
pickPlain (Plain x) = x
observed x = pickPair (Pair x undefined) + pickPlain (Plain x)
