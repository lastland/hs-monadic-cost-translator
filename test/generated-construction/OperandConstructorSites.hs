module OperandConstructorSites where

import qualified Test.QuickCheck as Q

data Cell a b = Cell { hard :: !a, soft :: b }

build :: a -> b -> b
build x y = case Cell x y of Cell _ result -> result

buildRecord :: a -> b -> b
buildRecord x y = soft (Cell {soft = y, hard = x})

replaceHard :: a -> b -> b
replaceHard x y = soft ((Cell False y) {hard = x})

replaceSoft :: Cell a b -> c -> c
replaceSoft cell y = soft (cell {soft = y})

prop_positional, prop_labelled, prop_update :: Q.Gen Bool -> Q.Property
prop_positional draw = build draw (Q.property True)
prop_labelled draw = buildRecord draw (Q.property True)
prop_update draw = replaceHard draw (Q.property True)

prop_lazy, prop_unchanged :: Q.Gen Bool -> Q.Property
prop_lazy draw = buildRecord True draw `seq` Q.property True
prop_unchanged draw = replaceSoft (Cell draw True) False `seq` Q.property True
