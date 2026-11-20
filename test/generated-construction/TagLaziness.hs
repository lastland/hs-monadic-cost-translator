{-# LANGUAGE MagicHash #-}
module TagLaziness (main) where

import GHC.Exts (Int(I#))
import GHC.Magic (dataToTag#)
import qualified Memorist.Memorist as M

data Choice a = This (M.Th a) | That (M.Th a)

-- Operational control only: explicit source primitives/IO do not have
-- construction permission. Both fields must remain unevaluated.
main :: IO ()
main = print
    (I# (dataToTag# (This undefined :: Choice Int)),
     I# (dataToTag# (That undefined :: Choice Int)))
