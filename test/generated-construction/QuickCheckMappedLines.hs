{-# LANGUAGE BangPatterns #-}
module QuickCheckMappedLines where

import qualified Test.QuickCheck as Q

-- LINE pragmas change reported locations, not which input token GHC checked.
claimLaw :: Q.Property -> Q.Property
{-# LINE 20 "Mapped.hs" #-}
claimLaw !_ = Q.property True

keepBool :: Bool -> Bool
{-# LINE 20 "Mapped.hs" #-}
keepBool !_ = True

data Cell a = Cell { field :: a }

genRole :: Cell (Q.Gen Bool) -> Q.Property
{-# LINE 40 "Mapped.hs" #-}
genRole c = field c `seq` Q.property True

boolUse :: Cell Bool -> Bool
{-# LINE 40 "Mapped.hs" #-}
boolUse c = field c `seq` True
