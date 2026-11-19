{-# LANGUAGE FlexibleInstances #-}
module MixedMakeInstances where

import QuickCheckRoles (Draw, Make(..))
import qualified Test.QuickCheck as Q

instance Make (Draw Bool) where
    make = Q.arbitrary

instance Make Char where
    make = (Q.arbitrary :: Draw Bool) `seq` 'x'
