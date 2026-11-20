{-# LANGUAGE PolyKinds, DataKinds, FlexibleInstances, TemplateHaskell,
    DerivingStrategies #-}
module THDuplicateGeneric where
import qualified Memorist.Memorist as M
import ManualGenerator (manualEq)

data Specialized (a :: k) = S (M.ThD Int) deriving stock (Eq, Show)
$(manualEq ''Specialized False)
duplicateEq :: Specialized 'True -> Specialized 'True -> Bool
duplicateEq = (==)
