module DemandScalarMethods where

import qualified Memorist.Memorist as M

data BoxD = BoxD (M.ThD Int) deriving (Eq, Show)

sameRuntime :: M.ThD Int -> M.ThD Int -> Bool
sameRuntime = (==)

showRuntime :: M.ThD Int -> String
showRuntime = show

sameHome :: BoxD -> BoxD -> Bool
sameHome = (==)

showHome :: BoxD -> String
showHome = show

differentRuntime :: M.ThD Int -> M.ThD Int -> Bool
differentRuntime = (/=)

listRuntime :: [M.ThD Int] -> ShowS
listRuntime = showList

precRuntime :: Int -> M.ThD Int -> ShowS
precRuntime = showsPrec

differentHome :: BoxD -> BoxD -> Bool
differentHome = (/=)

listHome :: [BoxD] -> ShowS
listHome = showList

precHome :: Int -> BoxD -> ShowS
precHome = showsPrec
