{-# LANGUAGE LambdaCase #-}
module Generated where

plainM x = helper x
  where
    helper y = (\case z -> z) y

recursiveM 0 = 0
recursiveM n = recursiveM (n - 1)

operatorM_60_43_62 x y = x + y

hiddenM x = x
