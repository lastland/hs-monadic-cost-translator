module Generated where

import qualified Example as Source
import qualified Memorist.Memorist as M
import Test.QuickCheck (Gen)

identityM :: M.Th Int -> M.MemoristM Int
identityM input = do
    M.tick
    M.force input

convert :: Source.Box -> Int
convert (Source.Box value) = value

genIdentityDemand :: Int -> Int -> Gen Int
genIdentityDemand _ value = pure value
