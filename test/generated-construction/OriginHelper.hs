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

partialLive :: [Int] -> [Int]
partialLive xs = Source.partial xs

partialDead :: [Int] -> [Int]
partialDead xs = if True then xs else Source.partial xs

partialCallback :: ([Int] -> [Int]) -> [Int]
partialCallback f = f []

passedPartial :: [Int]
passedPartial = partialCallback Source.partial

class ConcretePartial a where
    concretePartial :: a -> [Int]

instance ConcretePartial Bool where
    concretePartial _ = Source.partial []

class ConcretePartialDead a where
    concretePartialDead :: a -> [Int]

instance ConcretePartialDead Bool where
    concretePartialDead _ = if True then [] else Source.partial []
