module QuickCheckMixed where

import Data.Coerce (coerce)
import qualified Memorist.Memorist as M
import QuickCheckRoles (Draw, Verdict, Make(..), fieldLaw, sampleField)
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as G
import qualified Test.QuickCheck.Property as P
import Test.QuickCheck.Random (QCGen)
import Unsafe.Coerce (unsafeCoerce)

draw :: Draw (M.ThD Int)
draw = Q.sized sampleField

claim :: Verdict
claim = Q.forAll draw (\_ -> fieldLaw 1)

data Gen a = Gen a
newtype Property = Property Bool

fakeGen :: M.Th Int -> Gen Int
fakeGen input = Gen (M.tval input)

fakeProperty :: M.Th Bool -> Property
fakeProperty input = Property (M.tval input)

reuse, reuseProperty, rawGen, rawProperty :: M.MemoristM ()
reuse = (sampleField 4 :: Draw (M.ThD Int)) `seq` pure ()
reuseProperty = claim `seq` pure ()
rawGen = G.MkGen `seq` pure ()
rawProperty = P.MkProperty `seq` pure ()

extract :: QCGen -> Int -> M.ThD Int
extract = G.unGen draw

unwrap :: Draw Int -> QCGen -> Int -> Int
unwrap = coerce

cast :: Draw Int -> Int
cast = unsafeCoerce

class Ordinary a where
    observe :: a -> Bool
    observe _ = (Q.arbitrary :: Draw Int) `seq` True
instance Ordinary ()

instance Make Bool where
    make = (Q.arbitrary :: Draw Int) `seq` True

forceMade :: M.MemoristM ()
forceMade = (make :: Draw Int) `seq` pure ()
