{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
module OperandFamilies where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

type family F a where
    F Bool = Q.Gen Bool
    F Char = Char

h :: a -> F a -> Bool
h _ x = seq x True

law :: a -> F a -> Q.Property
law tag x = Q.property (h tag x)

type family Open a

forceOpen :: a -> Open a -> Bool
forceOpen _ x = seq x True

-- A downstream equation cannot give this declaration a property role.
upstream :: M.Th Bool -> Open Bool -> Open Bool
upstream t x = M.tval t `seq` x

generic :: a -> F a -> F a
generic _ x = seq x x

type family Strip a where
    Strip [a] = Strip a
    Strip Bool = Q.Property

strip :: a -> Strip a -> Bool
strip _ x = seq x True

-- GHC can check an abstract identity without unfolding this growing family.
type family Grow a where
    Grow a = Grow [a]

same :: a -> Grow a -> Grow a
same _ x = x
