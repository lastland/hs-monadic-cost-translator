{-# LANGUAGE UnicodeSyntax #-}
module Source (plain) where

plain ∷ Int → Int
plain x = helper x
  where
    helper y = y

recursive 0 = 0
recursive n = recursive (n - 1)

(<+>) x y = x + y

hidden x = x

-- Zero-argument FunBind values are deliberately outside the location rule.
shared = 5
