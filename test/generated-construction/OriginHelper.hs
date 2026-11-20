module OriginHelper where

import qualified Example as Source
import qualified Memorist.Memorist as M

keep :: Int -> Int
keep x = x

ordinary :: [Int] -> [Int]
ordinary xs = map keep xs

unused :: Int -> Int
unused x = if True then x else M.tval undefined

originLive :: Int -> Int
originLive x = Source.keep x

originDead :: Int -> Int
originDead x = if True then x else Source.keep x
