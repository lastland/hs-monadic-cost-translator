{-# LANGUAGE FlexibleInstances #-}
module DefaultLabelSelection where

import DefaultLabels (Labels(..))
import qualified Memorist.Memorist as M

instance Labels Char

inherited :: [Maybe Bool]
inherited = labels 'x'

data Switch = Switch (M.Th Bool)
instance Labels Switch where
    labels (Switch thunk) = [Just (M.tval thunk)]

overridden :: Switch -> [Maybe Bool]
overridden = labels

unresolved :: Labels a => a -> [Maybe Bool]
unresolved = labels

-- The class signature has a type variable here, but this field takes a function.
instance Labels (Bool -> Bool)

newtype Callback = Callback (Bool -> Bool)
instance Labels Callback
