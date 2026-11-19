{-# LANGUAGE TypeApplications #-}
module ExplicitMethodWrongCarrier where

ordinary :: [Int]
ordinary = pure @[] @Int 17
