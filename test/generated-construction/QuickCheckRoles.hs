{-# LANGUAGE FlexibleInstances #-}
module QuickCheckRoles where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Random

type Draw a = Random.Gen a
type Verdict = Random.Property

sampleField :: Random.Arbitrary a => Int -> Draw (M.ThD a)
sampleField size = if size <= 1 then pure M.Skip
    else Random.liftArbitrary (Random.resize (size - 2) Random.arbitrary)

fieldLaw :: Int -> Verdict
fieldLaw size = Random.forAll (sampleField size :: Draw (M.ThD Int)) $ \demand ->
    Random.property (size > 1 || demand == M.Skip)

class Samples a where
    draw :: a -> Draw Bool
    draw _ = Random.arbitrary
instance Samples ()

defaultLaw :: Verdict
defaultLaw = Random.forAll (draw ()) Random.property

class Make a where
    make :: a

instance Make (Random.Gen Int) where
    make = Random.chooseInt (0, 20)

instance Make Random.Property where
    make = Random.forAll (make :: Draw Int) $ \n -> n Random.=== n

data SamplesRecord = SamplesRecord { choices :: Draw Bool, statement :: Verdict }

recordLaw :: SamplesRecord -> Verdict
recordLaw r = Random.forAll (choices r) (\_ -> statement r)

ignoreChoices :: SamplesRecord -> Bool
ignoreChoices SamplesRecord{choices = _} = True
